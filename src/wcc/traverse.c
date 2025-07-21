#include "../config.h"
#include "wcc.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>  // memcpy

#include "ast.h"
#include "cc_misc.h"  // is_function_omitted
#include "fe_misc.h"  // curscope
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

const char VA_ARGS_NAME[] = ".._VA_ARGS";

Table builtin_function_table;

static Stmt *branching_stmt;

// GOTO label validation:
// Track if the previous statement was a block to validate label placement
static bool just_finished_block = false;

// Track labels visited during traversal to detect backward goto
static Table *visited_labels = NULL;

// Control flow stack for cross-branch goto detection
typedef struct ControlFrame {
  enum StmtKind kind;    // ST_FOR, ST_WHILE, ST_IF, ST_BLOCK, etc.
  Stmt *stmt;           // The control statement
  int depth;            // Nesting depth
  int enter_sequence;   // When this control structure was entered
  int exit_sequence;    // When this control structure was exited (-1 if not yet exited)
} ControlFrame;

static Vector *control_stack = NULL;  // Stack of ControlFrame

// Pending gotos with their control flow context
typedef struct PendingGoto {
  Stmt *goto_stmt;                    // The goto statement
  Vector *control_context;            // Copy of control_stack at goto time
  int sequence_number;                // Order in which this goto was encountered
} PendingGoto;

static Vector *pending_gotos = NULL;  // Vector of PendingGoto


// Validate that labels appear immediately after blocks for goto support
static void validate_label_placement(Stmt *stmt) {
  if (!just_finished_block) {
    parse_error(PE_NOFATAL, stmt->token,
                "Label '%.*s' must appear immediately after a block, for WebAssembly goto support",
                NAMES(stmt->token->ident));
  }
}

// Helper functions for control flow stack
static void push_control_frame(enum StmtKind kind, Stmt *stmt) {
  if (control_stack == NULL) {
    control_stack = new_vector();
  }
  
  ControlFrame *frame = malloc_or_die(sizeof(ControlFrame));
  frame->kind = kind;
  frame->stmt = stmt;
  frame->depth = control_stack->len;
  frame->enter_sequence = -1;  // No longer used
  frame->exit_sequence = -1;   // No longer used
  vec_push(control_stack, frame);
}

static void pop_control_frame(void) {
  if (control_stack != NULL && control_stack->len > 0) {
    ControlFrame *frame = vec_pop(control_stack);
    free(frame);
  }
}

static Vector *copy_control_stack(void) {
  if (control_stack == NULL) {
    return new_vector();
  }
  
  Vector *copy = new_vector();
  for (int i = 0; i < control_stack->len; ++i) {
    ControlFrame *orig = control_stack->data[i];
    ControlFrame *frame_copy = malloc_or_die(sizeof(ControlFrame));
    *frame_copy = *orig;
    vec_push(copy, frame_copy);
  }
  return copy;
}

static void free_control_context(Vector *context) {
  if (context != NULL) {
    for (int i = 0; i < context->len; ++i) {
      free(context->data[i]);
    }
    free(context);
  }
}

// Check if label_context is reachable from goto_context via valid WebAssembly control flow
static bool is_control_flow_valid(Vector *goto_context, Vector *label_context) {
  // Label context must be a prefix of goto context (you can only break outward)
  if (label_context->len > goto_context->len) {
    return false;  // Cannot jump inward to deeper nesting
  }
  
  // Check if label_context is a prefix of goto_context (label is parent of goto)
  for (int i = 0; i < label_context->len; ++i) {
    ControlFrame *goto_frame = goto_context->data[i];
    ControlFrame *label_frame = label_context->data[i];
    
    if (goto_frame->stmt != label_frame->stmt) {
      return false;  // Label is not a parent of goto - invalid cross-branch jump
    }
  }
  

  
  return true;  // Valid: label is a parent of goto (outward break)
}

// Check for backward goto - labels must appear before goto statements
static void validate_goto_direction(Stmt *stmt) {
  const Token *label_token = stmt->goto_.label;
  const Name *label_name = label_token->ident;
  
  // If the label has already been visited during traversal,
  // then this is a backward goto
  if (visited_labels != NULL && table_try_get(visited_labels, label_name, NULL)) {
    parse_error(PE_NOFATAL, stmt->token,
                "Backward goto not allowed: label '%.*s' appears before goto statement",
                NAMES(label_name));
    return;
  }
  
  // Record this goto with current control flow context for later validation
  if (pending_gotos == NULL) {
    pending_gotos = new_vector();
  }
  
  PendingGoto *pending = malloc_or_die(sizeof(PendingGoto));
  pending->goto_stmt = stmt;
  pending->control_context = copy_control_stack();
  pending->sequence_number = -1;  // No longer used
  vec_push(pending_gotos, pending);
}

// Validate pending gotos when a label is encountered
static void validate_pending_gotos_for_label(Stmt *label_stmt) {
  if (pending_gotos == NULL) {
    return;
  }
  
  const Name *label_name = label_stmt->token->ident;
  Vector *label_context = copy_control_stack();
  
  // Check all pending gotos that target this label
  for (int i = 0; i < pending_gotos->len; ++i) {
    PendingGoto *pending = pending_gotos->data[i];
    const Name *goto_target = pending->goto_stmt->goto_.label->ident;
    
    if (equal_name(goto_target, label_name)) {
              // This goto targets this label - validate control flow
        if (!is_control_flow_valid(pending->control_context, label_context)) {
        parse_error(PE_NOFATAL, pending->goto_stmt->token,
                    "Cross-branch goto not allowed: cannot jump between different control structures to label '%.*s'",
                    NAMES(label_name));
      }
      
      // Remove this pending goto (it's now resolved)
      free_control_context(pending->control_context);
      free(pending);
      vec_remove_at(pending_gotos, i);
      --i;  // Adjust index after removal
    }
  }
  
  free_control_context(label_context);
}

bool is_stack_param(const Type *type) {
  return !is_prim_type(type);
}

static void wasm_func_type(const Type *type, DataStorage *ds) {
  bool ret_param = type->func.ret->kind != TY_VOID && !is_prim_type(type->func.ret);
  const Vector *params = type->func.params;
  int param_count = 0;
  if (params != NULL) {
    for (int i = 0; i < params->len; ++i) {
      const Type *type = params->data[i];
      if (!is_stack_param(type))
        ++param_count;
    }
  }

  data_init(ds);
  data_reserve(ds, 3 + param_count + 3);

  data_uleb128(ds, -1, (int)ret_param + param_count + (type->func.vaargs ? 1 : 0));  // num params
  if (ret_param)
    data_push(ds, to_wtype(&tyVoidPtr));
  if (params != NULL) {
    for (int i = 0; i < params->len; ++i) {
      const Type *type = params->data[i];
      if (!is_stack_param(type))
        data_push(ds, to_wtype(type));
    }
  }
  if (type->func.vaargs)
    data_push(ds, to_wtype(&tyVoidPtr));  // vaarg pointer.

  if (type->func.ret->kind == TY_VOID) {
    data_push(ds, 0);  // num results
  } else if (ret_param) {
    data_push(ds, 1);  // num results
    data_push(ds, to_wtype(&tyVoidPtr));
  } else {
    data_push(ds, 1);  // num results
    data_push(ds, to_wtype(type->func.ret));
  }
}

int getsert_func_type_index(const Type *type, bool reg) {
  DataStorage ds;
  wasm_func_type(type, &ds);
  return getsert_func_type(ds.buf, ds.len, reg);
}

static FuncInfo *register_func_info(const Name *funcname, Function *func, VarInfo *varinfo,
                                    int flag) {
  assert(func == NULL || func->type->kind == TY_FUNC);
  FuncInfo *info;
  if (!table_try_get(&func_info_table, funcname, (void**)&info)) {
    info = calloc_or_die(sizeof(*info));
    table_put(&func_info_table, funcname, info);
    info->type_index = (uint32_t)-1;
    info->func_name = funcname;

    if (varinfo == NULL) {
      varinfo = scope_find(global_scope, funcname, NULL);
      assert(varinfo != NULL);
      assert(varinfo->type->kind == TY_FUNC);
      assert(func == NULL || same_type(varinfo->type, func->type));
    }
    info->varinfo = varinfo;
  }
  if (func != NULL)
    info->func = func;
  if (info->type_index == (uint32_t)-1)
    info->type_index = getsert_func_type_index(info->varinfo->type, true);
  info->flag |= flag;

  Table *attributes = NULL;
  if (func != NULL)
    attributes = func->attributes;
  if (attributes == NULL && varinfo != NULL && varinfo->global.funcdecl != NULL) {
    assert(varinfo->global.funcdecl->defun.func != NULL);
    attributes = varinfo->global.funcdecl->defun.func->attributes;
  }
  if (attributes != NULL) {
    const Vector *params;
    if (table_try_get(attributes, alloc_name("import_module", NULL, false), (void**)&params)) {
      const Token *token = params->len > 0 ? params->data[0] : NULL;
      if (params->len != 1 && token->kind != TK_STR)
        parse_error(PE_NOFATAL, token, "import_module: string expected");
      else
        info->module_name = alloc_name(token->str.buf, token->str.buf + token->str.len - 1, false);
    }
    if (table_try_get(attributes, alloc_name("import_name", NULL, false), (void**)&params)) {
      const Token *token = params->len > 0 ? params->data[0] : NULL;
      if (params->len != 1 && token->kind != TK_STR)
        parse_error(PE_NOFATAL, token, "import_name: string expected");
      else
        info->func_name = alloc_name(token->str.buf, token->str.buf + token->str.len - 1, false);
    }
    if (table_try_get(attributes, alloc_name("weak", NULL, false), (void**)&params))
      info->flag |= FF_WEAK;
  }

  return info;
}

static uint32_t register_indirect_function(const Name *name) {
  FuncInfo *info;
  if (table_try_get(&indirect_function_table, name, (void**)&info))
    return info->indirect_index;

  info = register_func_info(name, NULL, NULL, FF_INDIRECT | FF_REFERRED);
  uint32_t index = indirect_function_table.count;
  table_put(&indirect_function_table, name, info);
  getsert_indirect_function_table();
  return index;
}

GVarInfo *get_gvar_info(Expr *expr) {
  assert(expr->kind == EX_VAR);
  const Name *name = expr->var.name;
  Scope *scope;
  VarInfo *varinfo = scope_find(expr->var.scope, name, &scope);
  assert(varinfo != NULL && scope == expr->var.scope);
  if (!is_global_scope(scope)) {
    if (varinfo->storage & VS_EXTERN) {
      VarInfo *gvi = scope_find(global_scope, name, NULL);
      if (gvi == NULL) {
        // Register dummy.
        const Token *ident = alloc_ident(name, NULL, name->chars, name->chars + name->bytes);
        gvi = scope_add(global_scope, ident, expr->type, VS_EXTERN);
      }
      varinfo = gvi;
      scope = global_scope;
    } else if (varinfo->storage & VS_STATIC) {
      varinfo = varinfo->static_.svar;
      name = varinfo->ident->ident;
    }
  }
  assert(varinfo != NULL);
  GVarInfo *info = get_gvar_info_from_name(name);
  if (info == NULL && (!(varinfo->storage & VS_STATIC) || (varinfo->storage & VS_USED))) {
    // Returns dummy.
    info = register_gvar_info(name, varinfo);
    info->flag |= GVF_UNRESOLVED;
  }
  return info;
}

#define add_global_var(type, name)  scope_add(global_scope, name, type, VS_USED)

void add_builtin_function(const char *str, Type *type, BuiltinFunctionProc *proc,
                          bool add_to_scope) {
  const Name *name = alloc_name(str, NULL, false);
  table_put(&builtin_function_table, name, proc);

  if (add_to_scope)
    scope_add(global_scope, alloc_ident(name, NULL, name->chars, name->chars + name->bytes), type,
              0);
}

static void traverse_stmts(Vector *stmts);
static void traverse_stmt(Stmt *stmt);

// l=r  =>  (t=r, l=t)
static Expr *assign_to_tmp(Expr *assign, Expr **ptmp) {
  assert(assign->kind == EX_ASSIGN);
  const Token *token = assign->token;
  Type *type = assign->bop.lhs->type;
  Expr *rhs = assign->bop.rhs;
  Expr *tmp = alloc_tmp_var(curscope, type);
  *ptmp = tmp;
  Expr *assign_tmp = new_expr_bop(EX_ASSIGN, &tyVoid, token, tmp, rhs);
  assign->bop.rhs = tmp;
  return new_expr_bop(EX_COMMA, type, token, assign_tmp, assign);
}

static void traverse_expr(Expr **pexpr, bool needval);

static void traverse_func_expr(Expr **pexpr) {
  Expr *expr = *pexpr;
  Type *type = expr->type;
  assert(type->kind == TY_FUNC ||
         (type->kind == TY_PTR && type->pa.ptrof->kind == TY_FUNC));
  if (expr->kind == EX_VAR && type->kind == TY_FUNC) {  // Function must be placed in global scope.
    const Name *funcname = expr->var.name;
    if (!table_try_get(&builtin_function_table, funcname, NULL))
     register_func_info(funcname, NULL, NULL, FF_REFERRED);
  } else {
    type = get_callee_type(type);
    assert(type != NULL && type->kind == TY_FUNC);
    getsert_func_type_index(type, true);
    traverse_expr(pexpr, true);
    getsert_indirect_function_table();
  }
}

static void te_funcall(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;
  UNUSED(needval);

  Expr *func = expr->funcall.func;
  Type *functype = get_callee_type(func->type);
  if (functype == NULL) {
    parse_error(PE_NOFATAL, func->token, "Cannot call except function");
    return;
  }
  if (functype->func.ret->kind != TY_VOID && !is_prim_type(functype->func.ret)) {
    // Allocate local variable for return value.
    assert(curfunc != NULL);
    assert(curfunc->scopes != NULL);
    assert(curfunc->scopes->len > 0);
    VarInfo *varinfo = add_var_to_scope(curfunc->scopes->data[0], alloc_dummy_ident(),
                                        functype->func.ret, 0);
    FuncExtra *extra = curfunc->extra;
    assert(extra != NULL);
    if (extra->funcall_results == NULL)
      extra->funcall_results = new_vector();
    vec_push(extra->funcall_results, expr);
    vec_push(extra->funcall_results, varinfo);
  }

  Vector *args = expr->funcall.args;
  if (functype->func.params == NULL) {
    if (func->kind == EX_VAR) {
      // Extract function type again.
      VarInfo *varinfo = scope_find(func->var.scope, func->var.name, NULL);
      assert(varinfo != NULL);
      if (varinfo->type->kind == TY_FUNC) {
        func->type = functype = varinfo->type;
        if (functype->func.params != NULL)  // Updated.
          check_funcall_args(func, args, curscope);
      }
    }

    if (functype->func.params == NULL)
      parse_error(PE_NOFATAL, func->token, "function's parameters must be known");
  }

  size_t work_size = calc_funcall_work_size(expr);
  if (work_size > 0) {
    work_size = ALIGN(work_size, STACK_ALIGN);
    assert(curfunc != NULL);
    FuncInfo *finfo = table_get(&func_info_table, curfunc->ident->ident);
    assert(finfo != NULL);
    if (finfo->lspname == NULL) {
      const Token *lspident = alloc_dummy_ident();
      finfo->lspname = lspident->ident;

      assert(curfunc != NULL);
      assert(curfunc->scopes->len > 0);
      Scope *topscope = curfunc->scopes->data[0];
      scope_add(topscope, lspident, &tyVoidPtr, 0);
    }

    if (finfo->stack_work_size < work_size)
      finfo->stack_work_size = work_size;
  }

  traverse_func_expr(&expr->funcall.func);
  for (int i = 0, n = args->len; i < n; ++i)
    traverse_expr((Expr**)&args->data[i], true);

  if (func->kind == EX_VAR && func->type->kind == TY_FUNC) {  // Function must be placed in global scope.
    BuiltinFunctionProc *proc = table_get(&builtin_function_table, func->var.name);
    if (proc != NULL)
      (*proc)(expr, BFP_TRAVERSE);
  }
}

static void te_noop(Expr **pexpr, bool needval) { UNUSED(pexpr); UNUSED(needval); }

static void te_var(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;
  UNUSED(needval);
  if (expr->type->kind == TY_FUNC) {
    register_indirect_function(expr->var.name);
    traverse_func_expr(pexpr);
  } else {
    if (is_global_scope(expr->var.scope)) {
      // Register used global variable even if the entity is `extern`.
      get_gvar_info(expr);
    }
  }
}

static void te_bop(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;
  traverse_expr(&expr->bop.lhs, needval);
  traverse_expr(&expr->bop.rhs, needval);
}

static void te_logical(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;
  traverse_expr(&expr->bop.lhs, true);
  traverse_expr(&expr->bop.rhs, needval);
}

static void te_shift(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;
  // Make sure that RHS type is same as LHS.
  expr->bop.rhs = make_cast(expr->bop.lhs->type, expr->bop.rhs->token, expr->bop.rhs, false);
  traverse_expr(&expr->bop.lhs, needval);
  traverse_expr(&expr->bop.rhs, needval);
}

static void te_comma(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;
  traverse_expr(&expr->bop.lhs, false);
  traverse_expr(&expr->bop.rhs, needval);
}

static void te_assign(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;
  traverse_expr(&expr->bop.lhs, false);
  traverse_expr(&expr->bop.rhs, true);
  if (needval) {
    Expr *rhs = expr->bop.rhs;
    Type *type = rhs->type;
    if (!(is_const(rhs) || rhs->kind == EX_VAR)) {  // Rhs may have side effect.
      Expr *lhs = expr->bop.lhs;
      if (lhs->kind == EX_VAR) {
        type = lhs->type;
        rhs = lhs;
      } else {
        Expr *tmp;
        expr = assign_to_tmp(expr, &tmp);
        rhs = tmp;
      }
    }
    *pexpr = new_expr_bop(EX_COMMA, type, expr->token, expr, rhs);
  }
}

static void te_unary(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;
  traverse_expr(&expr->unary.sub, needval);
}

static void te_cast(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;
  traverse_expr(&expr->unary.sub, needval);
}

static void te_incdec(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;

  static const enum ExprKind kOpAddSub[2] = {EX_ADD, EX_SUB};
  traverse_expr(&expr->unary.sub, needval);
  Expr *target = expr->unary.sub;
  if (target->kind == EX_COMPLIT)
    target = target->complit.var;
  if (target->kind == EX_VAR) {
    VarInfo *varinfo = scope_find(target->var.scope, target->var.name, NULL);
    if (!(varinfo->storage & VS_REF_TAKEN) && !is_global_datsec_var(varinfo, target->var.scope))
      return;
  }

  Type *type = target->type;
  assert(is_number(type) || type->kind == TY_PTR);
  bool post = expr->kind >= EX_POSTINC;
  bool dec = (expr->kind - EX_PREINC) & 1;
  // (++xxx)  =>  (p = &xxx, tmp = *p + 1, *p = tmp, tmp)
  // (xxx++)  =>  (p = &xxx, tmp = *p, *p = tmp + 1, tmp)
  const Token *token = target->token;
  Type *ptrtype = ptrof(type);
  Expr *p = alloc_tmp_var(curscope, ptrtype);
  Expr *tmp = alloc_tmp_var(curscope, type);
  enum ExprKind op = kOpAddSub[dec];

  Expr *assign_p = new_expr_bop(EX_ASSIGN, &tyVoid, token, p, make_refer(token, target));
  Expr *deref_p = new_expr_deref(token, p);
  Expr *one = type->kind == TY_PTR ? new_expr_fixlit(&tySize, token, type_size(type->pa.ptrof))
#ifndef __NO_FLONUM
                                   : type->kind == TY_FLONUM ? new_expr_flolit(type, token, 1)
#endif
                                                             : new_expr_fixlit(type, token, 1);
  Expr *assign_tmp = new_expr_bop(EX_ASSIGN, &tyVoid, token, tmp,
                                  !post ? new_expr_bop(op, type, token, deref_p, one) : deref_p);
  Expr *assign_deref_p = new_expr_bop(EX_ASSIGN, &tyVoid, token, deref_p,
                                      !post ? tmp : new_expr_bop(op, type, token, tmp, one));

  *pexpr = new_expr_bop(EX_COMMA, type, token, assign_p,
                        new_expr_bop(EX_COMMA, type, token, assign_tmp,
                                     new_expr_bop(EX_COMMA, type, token, assign_deref_p, tmp)));
}

static void te_ternary(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;
  traverse_expr(&expr->ternary.cond, true);
  traverse_expr(&expr->ternary.tval, needval);
  traverse_expr(&expr->ternary.fval, needval);
}

static void te_member(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;
  traverse_expr(&expr->member.target, needval);
}

static void te_complit(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;
  UNUSED(needval);
  if (expr->complit.inits != NULL)
    traverse_stmts(expr->complit.inits);
}

static void te_block(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;
  UNUSED(needval);
  traverse_stmt(expr->block);
}

static void te_inlined(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;
  UNUSED(needval);
  Vector *args = expr->inlined.args;
  for (int i = 0; i < args->len; ++i)
    traverse_expr((Expr**)&args->data[i], true);
  traverse_stmt(expr->inlined.embedded);
}

static void traverse_expr(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;
  if (expr == NULL)
    return;

  typedef void (*TraverseExprFunc)(Expr **, bool);
  static const TraverseExprFunc table[] = {
    [EX_FIXNUM] = te_noop, [EX_FLONUM] = te_noop, [EX_STR] = te_noop, [EX_VAR] = te_var,
    [EX_ADD] = te_bop, [EX_SUB] = te_bop, [EX_MUL] = te_bop, [EX_DIV] = te_bop, [EX_MOD] = te_bop,
    [EX_BITAND] = te_bop, [EX_BITOR] = te_bop, [EX_BITXOR] = te_bop,
    [EX_EQ] = te_bop, [EX_NE] = te_bop, [EX_LT] = te_bop,
    [EX_LE] = te_bop, [EX_GE] = te_bop, [EX_GT] = te_bop,
    [EX_LOGAND] = te_logical, [EX_LOGIOR] = te_logical, [EX_LSHIFT] = te_shift, [EX_RSHIFT] = te_shift,
    [EX_POS] = te_unary, [EX_NEG] = te_unary, [EX_BITNOT] = te_unary,
    [EX_REF] = te_unary, [EX_DEREF] = te_unary,
    [EX_ASSIGN] = te_assign, [EX_COMMA] = te_comma,
    [EX_PREINC] = te_incdec, [EX_PREDEC] = te_incdec, [EX_POSTINC] = te_incdec, [EX_POSTDEC] = te_incdec,
    [EX_CAST] = te_cast, [EX_TERNARY] = te_ternary, [EX_MEMBER] = te_member,
    [EX_FUNCALL] = te_funcall, [EX_INLINED] = te_inlined, [EX_COMPLIT] = te_complit,
    [EX_BLOCK] = te_block,
  };

  assert(expr->kind < (int)ARRAY_SIZE(table));
  assert(table[expr->kind] != NULL);
  (*table[expr->kind])(pexpr, needval);
}

static void traverse_initializer(Initializer *init) {
  if (init == NULL)
    return;

  switch (init->kind) {
  case IK_SINGLE:
    traverse_expr(&init->single, true);
    break;
  case IK_MULTI:
    {
      Vector *multi = init->multi;
      for (int i = 0; i < multi->len; ++i)
        traverse_initializer(multi->data[i]);
    }
    break;
  case IK_DOT:
    traverse_initializer(init->dot.value);
    break;
  case IK_BRKT:
    traverse_initializer(init->bracket.value);
    break;
  }
}

static Stmt *push_branching_stmt(Stmt *stmt) {
  Stmt *prev = branching_stmt;
  branching_stmt = stmt;
  return prev;
}

static void traverse_if(Stmt *stmt) {
  traverse_expr(&stmt->if_.cond, true);
  Stmt *saved = push_branching_stmt(stmt);
  traverse_stmt(stmt->if_.tblock);
  traverse_stmt(stmt->if_.fblock);
  branching_stmt = saved;
}

static void traverse_switch(Stmt *stmt) {
  traverse_expr(&stmt->switch_.value, true);
  Stmt *saved = push_branching_stmt(stmt);
  traverse_stmt(stmt->switch_.body);
  branching_stmt = saved;

  if (!is_const(stmt->switch_.value) && stmt->switch_.value->kind != EX_VAR) {
    Expr *org_value = stmt->switch_.value;
    // Store value into temporary variable.
    assert(curfunc != NULL);
    Scope *scope = curfunc->scopes->data[0];
    const Token *ident = alloc_dummy_ident();
    Type *type = stmt->switch_.value->type;
    scope_add(scope, ident, type, 0);

    // switch (complex)  =>  switch ((tmp = complex, tmp))
    Expr *var = new_expr_variable(ident->ident, type, NULL, scope);
    Expr *comma = new_expr_bop(
        EX_COMMA, type, org_value->token,
        new_expr_bop(EX_ASSIGN, &tyVoid, org_value->token, var, org_value),
        var);
    stmt->switch_.value = comma;
  }
}

static void traverse_case(Stmt *stmt) {
  if (branching_stmt->kind != ST_SWITCH)
    parse_error(PE_FATAL, stmt->token, "case/default inside branch not supported");
  traverse_stmt(stmt->case_.stmt);
}

static void traverse_while(Stmt *stmt) {
  traverse_expr(&stmt->while_.cond, true);
  Stmt *saved = push_branching_stmt(stmt);
  traverse_stmt(stmt->while_.body);
  branching_stmt = saved;
}

static void traverse_do_while(Stmt *stmt) {
  Stmt *saved = push_branching_stmt(stmt);
  traverse_stmt(stmt->while_.body);
  branching_stmt = saved;
  traverse_expr(&stmt->while_.cond, true);
}

static void traverse_for(Stmt *stmt) {
  traverse_expr(&stmt->for_.pre, false);
  traverse_expr(&stmt->for_.cond, true);
  traverse_expr(&stmt->for_.post, false);
  Stmt *saved = push_branching_stmt(stmt);
  traverse_stmt(stmt->for_.body);
  branching_stmt = saved;
}

static void traverse_varinfo(VarInfo *varinfo) {
  if (varinfo->type->kind == TY_FUNC) {
    // Local extern function declaration.
    register_func_info(varinfo->ident->ident, NULL, varinfo, 0);
  } else if (varinfo->storage & VS_EXTERN) {
    assert(!is_global_scope(curscope));
    if (scope_find(global_scope, varinfo->ident->ident, NULL) == NULL) {
      // Register into global to output linking information.
      GVarInfo *info = register_gvar_info(varinfo->ident->ident, varinfo);
      if (info != NULL)
        info->flag |= GVF_UNRESOLVED;
    }
  }
  if (!(varinfo->storage & (VS_EXTERN | VS_STATIC | VS_ENUM_MEMBER)))
    traverse_initializer(varinfo->local.init);
}

static void traverse_scope(Scope *scope) {
  Vector *vars = scope->vars;
  for (int i = 0, len = vars->len; i < len; ++i) {
    VarInfo *varinfo = vars->data[i];
    traverse_varinfo(varinfo);
  }
}

static void traverse_vardecl(VarDecl *decl) {
  traverse_varinfo(decl->varinfo);
  traverse_stmt(decl->init_stmt);
}

static void traverse_stmt(Stmt *stmt) {
  if (stmt == NULL)
    return;

  // goto support: track if previous statement was a block
  // Labels must appear immediately after blocks
  if (stmt->kind != ST_LABEL) {
    just_finished_block = false;
  }

  switch (stmt->kind) {
  case ST_EMPTY: break;
  case ST_EXPR:  traverse_expr(&stmt->expr, false); break;
  case ST_RETURN:
    traverse_expr(&stmt->return_.val, true);
    break;
  case ST_BLOCK:
    {
      push_control_frame(ST_BLOCK, stmt);
      Scope *bak = NULL;
      if (stmt->block.scope != NULL) {
        bak = curscope;
        curscope = stmt->block.scope;
        traverse_scope(curscope);
      }
      traverse_stmts(stmt->block.stmts);
      if (bak != NULL)
        curscope = bak;
      pop_control_frame();
      just_finished_block = true;
    }
    break;
  case ST_IF:
    push_control_frame(ST_IF, stmt);
    traverse_if(stmt);
    pop_control_frame();
    just_finished_block = true;
    break;
  case ST_SWITCH:
    push_control_frame(ST_SWITCH, stmt);
    traverse_switch(stmt);
    pop_control_frame();
    just_finished_block = true;
    break;
  case ST_CASE: traverse_case(stmt); break;
  case ST_WHILE:
    push_control_frame(ST_WHILE, stmt);
    traverse_while(stmt);
    pop_control_frame();
    just_finished_block = true;
    break;
  case ST_DO_WHILE:
    push_control_frame(ST_DO_WHILE, stmt);
    traverse_do_while(stmt);
    pop_control_frame();
    just_finished_block = true;
    break;
  case ST_FOR:
    push_control_frame(ST_FOR, stmt);
    traverse_for(stmt);
    pop_control_frame();
    just_finished_block = true;
    break;
  case ST_BREAK:  break;
  case ST_CONTINUE:  break;
  case ST_GOTO:
    validate_goto_direction(stmt);
    break;
  case ST_LABEL:
    validate_label_placement(stmt);
    // Mark this label as visited for backward goto detection
    if (visited_labels != NULL) {
      table_put(visited_labels, stmt->token->ident, stmt);
    }
    // Validate pending gotos that target this label
    validate_pending_gotos_for_label(stmt);
    traverse_stmt(stmt->label.stmt);
    break;
  case ST_VARDECL:  traverse_vardecl(stmt->vardecl); break;
  case ST_ASM:  break;
  }
}

static void traverse_stmts(Vector *stmts) {
  assert(stmts != NULL);
  for (int i = 0, len = stmts->len; i < len; ++i) {
    Stmt *stmt = stmts->data[i];
    traverse_stmt(stmt);
  }
}

static void modify_func_name(Function *func) {
  static const Name *main_name;
  if (main_name == NULL)
    main_name = alloc_name("main", NULL, false);
  if (!equal_name(func->ident->ident, main_name))
    return;

  assert(func->params != NULL);
  Type *functype = func->type;
  const Name *newname = NULL;
  switch (func->params->len) {
  case 0:
    newname = alloc_name("__main_void", NULL, false);
    break;
  case 2:
    newname = alloc_name("__main_argc_argv", NULL, false);
    break;
  default:
    error("main function must take no argument or two arguments");
    break;
  }

  VarInfo *vi = scope_find(global_scope, newname, NULL);
  if (vi != NULL) {
    const Token *token = func->body_block != NULL ? func->body_block->token : NULL;
    parse_error(PE_NOFATAL, token, "`%.*s' function already defined", NAMES(newname));
    return;
  }

  // Rename two arguments `main` to `__main_argc_argv`.
  VarInfo *org_varinfo = scope_find(global_scope, main_name, NULL);
  assert(org_varinfo != NULL);
  func->ident = alloc_ident(newname, NULL, newname->chars, newname->chars + newname->bytes);
  VarInfo *varinfo = scope_add(global_scope, func->ident, functype, org_varinfo->storage);
  varinfo->global.func = func;

  // Clear `main` function.
  assert(org_varinfo->global.func == func);
  org_varinfo->global.func = NULL;
}

static void traverse_defun(Function *func) {
  if (func->scopes == NULL)  // Prototype definition
    return;
  VarInfo *funcvi = scope_find(global_scope, func->ident->ident, NULL);
  if (!(funcvi->storage & VS_USED) && is_function_omitted(funcvi))
    return;

  // Static variables.
  Vector *static_vars = func->static_vars;
  if (static_vars != NULL) {
    VarInfo *funcvi = scope_find(global_scope, func->ident->ident, NULL);
    assert(funcvi != NULL);
    int k = (funcvi->storage & (VS_STATIC | VS_USED)) == VS_STATIC ? 1 : 0;  // Static function but not used.
    for (; k < 2; ++k) {  // 0=register, 1=traverse
      for (int i = 0, len = static_vars->len; i < len; ++i) {
        VarInfo *varinfo = static_vars->data[i];
        assert(!(varinfo->storage & (VS_EXTERN | VS_ENUM_MEMBER) || varinfo->type->kind == TY_FUNC));
        if ((varinfo->storage & (VS_STATIC | VS_USED)) == VS_STATIC)  // Static variable but not used.
          continue;
        if (k == 0)
          register_gvar_info(varinfo->ident->ident, varinfo);
        else
          traverse_initializer(varinfo->global.init);
      }
    }
  }

  FuncExtra *extra = calloc_or_die(sizeof(FuncExtra));
  extra->reloc_code = new_vector();
  func->extra = extra;

  modify_func_name(func);

  Type *functype = func->type;
  if (functype->func.vaargs) {
    Type *tyvalist = find_typedef(curscope, alloc_name("__builtin_va_list", NULL, false), NULL);
    assert(tyvalist != NULL);

    const Name *name = alloc_name(VA_ARGS_NAME, NULL, false);
    scope_add(func->scopes->data[0],
              alloc_ident(name, NULL, name->chars, name->chars + name->bytes), tyvalist, 0);
  }

  register_func_info(func->ident->ident, func, NULL, 0);
  curfunc = func;
  
  // Initialize visited labels table for backward goto detection
  visited_labels = malloc_or_die(sizeof(Table));
  table_init(visited_labels);
  
  // Initialize control flow tracking
  control_stack = new_vector();
  pending_gotos = new_vector();
  
  traverse_stmt(func->body_block);
  
  // Check for any unresolved gotos
  if (pending_gotos->len > 0) {
    for (int i = 0; i < pending_gotos->len; ++i) {
      PendingGoto *pending = pending_gotos->data[i];
      const Name *target_name = pending->goto_stmt->goto_.label->ident;
      parse_error(PE_NOFATAL, pending->goto_stmt->token,
                  "Label '%.*s' not found", NAMES(target_name));
      free_control_context(pending->control_context);
      free(pending);
    }
  }
  
  // Cleanup control flow tracking
  if (control_stack != NULL) {
    for (int i = 0; i < control_stack->len; ++i) {
      free(control_stack->data[i]);
    }
    free(control_stack);
    control_stack = NULL;
  }
  
  if (pending_gotos != NULL) {
    free(pending_gotos);
    pending_gotos = NULL;
  }

  // Cleanup visited labels table
  if (visited_labels->entries != NULL) {
    free(visited_labels->entries);
  }
  free(visited_labels);
  visited_labels = NULL;
  
  curfunc = NULL;

  // Static variables are traversed through global variables.

  // Pick up constructor function.
  // Destructor is converted to constructor which register itself with atexit,
  // so it is not necessary to pick up destructor here.
  const Name *constructor_name = alloc_name("constructor", NULL, false);
  if (func->attributes != NULL) {
    if (table_try_get(func->attributes, constructor_name, NULL)) {
      // Ensure that the function has no parameters and returns void.
      const Type *type = func->type;
      if (type->func.params == NULL || type->func.params->len > 0 ||
          type->func.ret->kind != TY_VOID) {
        const Token *token = func->body_block != NULL ? func->body_block->token : NULL;
        parse_error(PE_NOFATAL, token, "constructor must have no parameters and return void");
      } else {
        if (init_funcs == NULL)
          init_funcs = new_vector();
        vec_push(init_funcs, func);
      }
    }
  }
}

static void traverse_decl(Declaration *decl) {
  if (decl == NULL)
    return;

  switch (decl->kind) {
  case DCL_DEFUN:
    traverse_defun(decl->defun.func);
    break;
  case DCL_ASM:
    parse_error(PE_NOFATAL, decl->asmstr->token, "`__asm` not allowed");
    break;
  }
}

static void add_builtins(int flag) {
  if (flag & CUF_USE_SP) {
    const Name *name = alloc_name(SP_NAME, NULL, false);
    VarInfo *varinfo = scope_find(global_scope, name, NULL);
    if (varinfo == NULL) {
      varinfo = add_global_var(&tyVoidPtr,
                               alloc_ident(name, NULL, name->chars, name->chars + name->bytes));
    } else {
      if (!same_type(varinfo->type, &tyVoidPtr))
        parse_error(PE_NOFATAL, NULL, "Illegal type: %.*s", NAMES(name));
    }
    GVarInfo *info = get_gvar_info_from_name(name);
    if (info == NULL)
      info = register_gvar_info(name, varinfo);
    assert(info != NULL);
    info->flag |= GVF_UNRESOLVED;
  }
}

static bool detect_compile_unit_sp(Function *func) {
  FuncInfo *finfo = table_get(&func_info_table, func->ident->ident);
  assert(finfo != NULL);
  if (finfo->flag & FF_STACK_MODIFIED)
    return true;
  FuncExtra *extra = func->extra;
  assert(extra != NULL);
  if (extra->setjmp_count > 0)
    return true;
  if (finfo->lspname != NULL)
    return true;

  Vector *scopes = func->scopes;
  if (scopes == NULL)  // Prototype definition
    return false;

  const Type *functype = func->type;
  unsigned int param_count = functype->func.params != NULL ? functype->func.params->len : 0;
  unsigned int pparam_count = 0;  // Primitive parameter count

  for (int i = 0; i < scopes->len; ++i) {
    Scope *scope = scopes->data[i];
    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (!is_local_storage(varinfo)) {
        // Static entity is allocated in global, not on stack.
        // Extern doesn't have its entity.
        // Enum members are replaced to constant value.
        continue;
      }

      int param_index = -1;
      if (i == 0 && param_count > 0) {
        int k = get_funparam_index(func, varinfo->ident->ident);
        if (k >= 0) {
          param_index = k;
          if (!is_stack_param(varinfo->type))
            ++pparam_count;
        }
      }
      if ((varinfo->storage & VS_REF_TAKEN) || (is_stack_param(varinfo->type) && param_index < 0)) {
        return true;
      }
    }
  }
  return pparam_count != param_count;
}

static bool detect_func_use_memory(Function *func) {
  Vector *scopes = func->scopes;
  if (scopes != NULL) {
    for (int i = 0; i < scopes->len; ++i) {
      Scope *scope = scopes->data[i];
      for (int j = 0; j < scope->vars->len; ++j) {
        VarInfo *varinfo = scope->vars->data[j];
        if ((varinfo->storage & (VS_STATIC | VS_USED)) == (VS_STATIC | VS_USED))
          return true;
      }
    }
  }
  return false;
}

static int detect_compile_unit_memory(void) {
  for (int i = 0, len = global_scope->vars->len; i < len; ++i) {
    VarInfo *varinfo = global_scope->vars->data[i];
    if (varinfo->type->kind == TY_FUNC) {
      Function *func = varinfo->global.func;
      if (func != NULL && detect_func_use_memory(func))
        return CUF_LINEAR_MEMORY;
      continue;
    }
    if ((varinfo->storage & (VS_EXTERN | VS_ENUM_MEMBER)) ||
        (varinfo->storage & (VS_STATIC | VS_USED)) == VS_STATIC)  // Static variable but not used.
      continue;
    if (is_global_datsec_var(varinfo, global_scope))
      return CUF_LINEAR_MEMORY;
  }
  return 0;
}

static int detect_compile_unit_flags(Vector *decls) {
  int flag = detect_compile_unit_memory();

  for (int i = 0, len = decls->len; i < len; ++i) {
    Declaration *decl = decls->data[i];
    if (decl->kind != DCL_DEFUN)
      continue;

    Function *func = decl->defun.func;
    VarInfo *funcvi = scope_find(global_scope, func->ident->ident, NULL);
    if (is_function_omitted(funcvi))
      continue;

    if (detect_compile_unit_sp(func)) {
      flag |= CUF_USE_SP;
      break;
    }
  }

  return flag;
}

void traverse_ast(Vector *decls) {
  compile_unit_flag = 0;

  // Global scope
  for (int k = 0; k < 2; ++k) {  // 0=register, 1=traverse
    for (int i = 0, len = global_scope->vars->len; i < len; ++i) {
      VarInfo *varinfo = global_scope->vars->data[i];
      int storage = varinfo->storage;
      if (varinfo->type->kind == TY_FUNC ||
          (storage & (VS_EXTERN | VS_ENUM_MEMBER)) ||
          (storage & (VS_STATIC | VS_USED)) == VS_STATIC)  // Static variable but not used.
        continue;
      if (k == 0)
        register_gvar_info(varinfo->ident->ident, varinfo);
      else
        traverse_initializer(varinfo->global.init);
    }
  }

  for (int i = 0, len = decls->len; i < len; ++i) {
    Declaration *decl = decls->data[i];
    traverse_decl(decl);
  }

  compile_unit_flag |= detect_compile_unit_flags(decls);

  add_builtins(compile_unit_flag);

  // traverse_ast_for_setjmp();
  for (int i = 0, len = decls->len; i < len; ++i) {
    Declaration *decl = decls->data[i];
    if (decl->kind != DCL_DEFUN)
      continue;
    Function *func = decl->defun.func;
    VarInfo *funcvi = scope_find(global_scope, func->ident->ident, NULL);
    if (!(funcvi->storage & VS_USED) && is_function_omitted(funcvi))
      continue;
    int count = ((FuncExtra*)func->extra)->setjmp_count;
    if (count > 0) {
      curfunc = func;
      modify_ast_for_setjmp(count);
      curfunc = NULL;
    }
  }

  // Indirect functions.
  {
    const Name *name;
    FuncInfo *info;
    uint32_t index = INDIRECT_FUNCTION_TABLE_START_INDEX;
    for (int it = 0;
         (it = table_iterate(&indirect_function_table, it, &name, (void**)&info)) != -1; )
      info->indirect_index = index++;
  }

  {
    uint32_t symbol_index = 0;
    const Name *name;
    FuncInfo *info;
    for (int it = 0; (it = table_iterate(&func_info_table, it, &name, (void**)&info)) != -1; ) {
      if (info->flag == 0 && info->func == NULL)
        continue;
      if (is_function_omitted(info->varinfo))
        continue;
      ++symbol_index;
    }

    // Assign linking index to globals.
    uint32_t global_index = 0;
    uint32_t data_index = 0;
    for (int k = 0; k < 3; ++k) {  // 0=unresolved, 1=resolved(data), 2=resolved(bss)
      static const char *kTitle[] = {"import", "data", "bss"};
      VERBOSE("### Globals(%s)\n", kTitle[k]);
      GVarInfo *info;
      for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
        const VarInfo *varinfo = info->varinfo;
        assert(!(varinfo->storage & VS_ENUM_MEMBER || varinfo->type->kind == TY_FUNC));
        assert(!((varinfo->storage & (VS_STATIC | VS_USED)) == VS_STATIC));
        if ((k == 0 && !(info->flag & GVF_UNRESOLVED)) ||
            (k != 0 && ((info->flag & GVF_UNRESOLVED) || (varinfo->global.init == NULL) == (k == 1))))
          continue;
        if (!is_global_datsec_var(varinfo, global_scope)) {
          info->item_index = info->prim.index = global_index++;
        } else if (!(varinfo->storage & VS_EXTERN)) {
          info->item_index = data_index++;
        } else {
          info->item_index = (uint32_t)-1;
        }
        info->symbol_index = symbol_index++;
        VERBOSE("%2d: %.*s (%d)\n", info->item_index, NAMES(varinfo->ident->ident),
                info->symbol_index);
      }
    }

    // Table
    for (int i = 0, len = tables->len; i < len; ++i) {
      TableInfo *ti = tables->data[i];
      ti->symbol_index = symbol_index++;
    }

    // Tag
    for (int i = 0, len = tags->len; i < len; ++i) {
      TagInfo *ti = tags->data[i];
      ti->symbol_index = symbol_index++;
    }
  }

  {
    // Enumerate functions.
    VERBOSES("### Functions\n");
    const Name *name;
    FuncInfo *info;
    int32_t index = 0;
    for (int k = 0; k < 2; ++k) {  // 0: import, 1: defined-and-referred
      for (int it = 0; (it = table_iterate(&func_info_table, it, &name, (void**)&info)) != -1; ) {
        if ((k == 0 && (info->func != NULL || info->flag == 0)) ||  // Put external function first.
            (k == 1 && info->func == NULL))                         // Defined function later.
          continue;
        if (is_function_omitted(info->varinfo))
          continue;
        info->index = index++;
        VERBOSE("%2d: %.*s%s\n", info->index, NAMES(name), k == 0 ? "  (import)" : "");
      }
    }
    VERBOSES("\n");
  }

  {
    // Enumerate global variables.
    const uint32_t START_ADDRESS = 0;  // Physical address is assigned by linker, so start from 0 here.
    uint32_t address = START_ADDRESS;

    VERBOSE("### Memory  0x%x\n", address);
    for (int k = 0; k < 2; ++k) {  // 0: data, 1: bss
      if (k == 1)
        VERBOSE("---- BSS  0x%x\n", address);
      const Name *name;
      GVarInfo *info;
      for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
        const VarInfo *varinfo = info->varinfo;
        int storage = varinfo->storage;
        if (varinfo->type->kind == TY_FUNC ||
            (storage & (VS_EXTERN | VS_ENUM_MEMBER)) ||
            (storage & (VS_STATIC | VS_USED)) == VS_STATIC)  // Static variable but not used.
          continue;
        if ((varinfo->global.init == NULL) == (k == 0) ||
            !is_global_datsec_var(varinfo, global_scope))
          continue;

        // Mapped to memory
        address = ALIGN(address, align_size(varinfo->type));
        info->non_prim.address = address;
        size_t size = type_size(varinfo->type);
        address += size;
        VERBOSE("%04x: %.*s  (size=0x%zx)\n", info->non_prim.address, NAMES(varinfo->ident->ident),
                size);
      }
    }
  }
}

