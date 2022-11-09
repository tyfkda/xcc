#include "wcc.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>  // memcpy

#include "ast.h"
#include "lexer.h"
#include "parser.h"  // curscope
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

const char SP_NAME[] = "$_SP";  // Hidden variable name for stack pointer (global).
const char BREAK_ADDRESS_NAME[] = "__curbrk";
const char MEMORY_PAGE_COUNT_NAME[] = "__memoryPageCount";
const char MEMCPY_NAME[] = "_memcpy";
const char MEMSET_NAME[] = "_memset";
const char VA_ARGS_NAME[] = ".._VA_ARGS";

Table func_info_table;
Table gvar_info_table;
Table builtin_function_table;
Vector *functypes;
Table indirect_function_table;

static Stmt *branching_stmt;

bool is_prim_type(const Type *type) {
  return is_number(type) || type->kind == TY_PTR;
}

bool is_stack_param(const Type *type) {
  return !is_prim_type(type);
}

static WasmFuncType *wasm_func_type(const Type *type) {
  bool ret_param = type->func.ret->kind != TY_VOID && !is_prim_type(type->func.ret);
  const Vector *params = type->func.params;
  int param_count = 0;
  if (params != NULL) {
    for (int i = 0; i < params->len; ++i) {
      VarInfo *varinfo = params->data[i];
      if (!is_stack_param(varinfo->type))
        ++param_count;
    }
  }

  DataStorage d;
  data_init(&d);
  data_reserve(&d, 3 + param_count + 3);

  emit_uleb128(&d, -1, (int)ret_param + param_count + (type->func.vaargs ? 1 : 0));  // num params
  if (ret_param)
    data_push(&d, to_wtype(&tyVoidPtr));
  if (params != NULL) {
    for (int i = 0; i < params->len; ++i) {
      VarInfo *varinfo = params->data[i];
      if (!is_stack_param(varinfo->type))
        data_push(&d, to_wtype(varinfo->type));
    }
  }
  if (type->func.vaargs)
    data_push(&d, to_wtype(&tyVoidPtr));  // vaarg pointer.

  if (type->func.ret->kind == TY_VOID) {
    data_push(&d, 0);  // num results
  } else if (ret_param) {
    data_push(&d, 1);  // num results
    data_push(&d, to_wtype(&tyVoidPtr));
  } else {
    data_push(&d, 1);  // num results
    data_push(&d, to_wtype(type->func.ret));
  }

  WasmFuncType *t = malloc(sizeof(*t) - sizeof(t->buf) + d.len);
  t->size = d.len;
  memcpy(t->buf, d.buf, d.len);

  data_release(&d);
  return t;
}

static int getsert_func_type_index(const Type *type, bool reg) {
  WasmFuncType *wt = wasm_func_type(type);
  int len = functypes->len;
  for (int i = 0; i < len; ++i) {
    const WasmFuncType *t = functypes->data[i];
    if (wt->size == t->size && memcmp(wt->buf, t->buf, wt->size) == 0)
      return i;
  }
  if (reg) {
    int index = functypes->len;
    vec_push(functypes, wt);
    return index;
  }
  return -1;
}

int get_func_type_index(const Type *type) {
  return getsert_func_type_index(type, false);
}

static FuncInfo *register_func_info(const Name *funcname, Function *func, const Type *type, int flag) {
  FuncInfo *info;
  if (!table_try_get(&func_info_table, funcname, (void**)&info)) {
    info = calloc(1, sizeof(*info));
    table_put(&func_info_table, funcname, info);
    info->type_index = (uint32_t)-1;
  }
  if (func != NULL)
    info->func = func;
  if (type != NULL) {
    info->type = type;
    if (info->type_index == (uint32_t)-1)
      info->type_index = getsert_func_type_index(type, true);
  }
  info->flag |= flag;
  return info;
}

static void register_func_info_if_not_exist(const Name *funcname, Type *(*callback)(void)) {
  if (table_get(&func_info_table, funcname) == NULL) {
    Type *functype = (*callback)();
    register_func_info(funcname, NULL, functype, FF_REFERED);
    scope_add(global_scope, funcname, functype, 0);
  }
}

static Type *gen_memcpy_func_type(void) {
  Type *rettype = &tyVoid;  // Differ from memcpy, no return value.
  Vector *params = new_vector();
  var_add(params, NULL, &tyVoidPtr, 0);
  var_add(params, NULL, &tyVoidPtr, 0);
  var_add(params, NULL, &tySize, 0);
  Vector *param_types = extract_varinfo_types(params);
  return new_func_type(rettype, params, param_types, false);
}

static void register_func_info_memcpy(void) {
  const Name *funcname = alloc_name(MEMCPY_NAME, NULL, false);
  register_func_info_if_not_exist(funcname, gen_memcpy_func_type);
}

static Type *gen_memset_func_type(void) {
  Type *rettype = &tyVoid;  // Differ from memset, no return value.
  Vector *params = new_vector();
  var_add(params, NULL, &tyVoidPtr, 0);
  var_add(params, NULL, &tyInt, 0);
  var_add(params, NULL, &tySize, 0);
  Vector *param_types = extract_varinfo_types(params);
  return new_func_type(rettype, params, param_types, false);
}

static void register_func_info_memset(void) {
  const Name *funcname = alloc_name(MEMSET_NAME, NULL, false);
  register_func_info_if_not_exist(funcname, gen_memset_func_type);
}

static uint32_t register_indirect_function(const Name *name, const Type *type) {
  FuncInfo *info;
  if (table_try_get(&indirect_function_table, name, (void**)&info))
    return info->indirect_index;

  info = register_func_info(name, NULL, type, FF_INDIRECT);
  uint32_t index = indirect_function_table.count + 1;
  info->indirect_index = index;
  table_put(&indirect_function_table, name, info);
  return index;
}

uint32_t get_indirect_function_index(const Name *name) {
  FuncInfo *info = table_get(&indirect_function_table, name);
  assert(info != NULL && info->indirect_index > 0);
  return info->indirect_index;
}

static GVarInfo *register_gvar_info(const Name *name, VarInfo *varinfo) {
#if !defined(NDEBUG)
  assert(!table_try_get(&gvar_info_table, name, NULL));
#endif
  GVarInfo *info = calloc(1, sizeof(*info));
  info->varinfo = varinfo;
  info->export = false;
  table_put(&gvar_info_table, name, info);
  return info;
}

GVarInfo *get_gvar_info_from_name(const Name *name) {
  return table_get(&gvar_info_table, name);
}

GVarInfo *get_gvar_info(Expr *expr) {
  assert(expr->kind == EX_VAR);
  Scope *scope;
  VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
  assert(varinfo != NULL && scope == expr->var.scope);
  if (!is_global_scope(scope)) {
    if (varinfo->storage & VS_EXTERN) {
      varinfo = scope_find(scope = global_scope, expr->var.name, &scope);
    } else if (varinfo->storage & VS_STATIC) {
      varinfo = varinfo->static_.gvar;
    }
  }
  GVarInfo *info = NULL;
  if (varinfo == NULL) {
    parse_error(PE_FATAL, expr->token, "Variable not found: %.*s", expr->var.name->bytes, expr->var.name->chars);
  } else {
    info = get_gvar_info_from_name(varinfo->name);
    if (info == NULL) {
      fprintf(stderr, "Global variable not found: %.*s\n", expr->var.name->bytes, expr->var.name->chars);
      ++compile_error_count;
      // Returns dummy.
      info = register_gvar_info(varinfo->name, varinfo);
    }
  }
  return info;
}

static GVarInfo *add_global_var(Type *type, const Name *name) {
  VarInfo *varinfo = scope_add(global_scope, name, type, 0);
  return register_gvar_info(name, varinfo);
}

void add_builtin_function(const char *str, Type *type, BuiltinFunctionProc *proc, bool add_to_scope) {
  const Name *name = alloc_name(str, NULL, false);
  table_put(&builtin_function_table, name, proc);

  if (add_to_scope)
    scope_add(global_scope, name, type, 0);
}

static void traverse_stmts(Vector *stmts);
static void traverse_stmt(Stmt *stmt);

static Expr *alloc_tmp(const Token *token, Type *type) {
  const Name *name = alloc_label();
  scope_add(curscope, name, type, 0);
  return new_expr_variable(name, type, token, curscope);
}

// l=r  =>  (t=r, l=t)
static Expr *assign_to_tmp(Expr *assign, Expr *bop) {
  const Token *token = assign->token;
  Type *type = bop->bop.lhs->type;
  Expr *rhs = bop->bop.rhs;
  Expr *tmp = alloc_tmp(token, type);
  Expr *assign_tmp = new_expr_bop(EX_ASSIGN, &tyVoid, token, tmp, rhs);
  bop->bop.rhs = tmp;
  return new_expr_bop(EX_COMMA, type, token, assign_tmp, assign);
}

static void traverse_expr(Expr **pexpr, bool needval);

static void traverse_func_expr(Expr **pexpr) {
  Expr *expr = *pexpr;
  const Type *type = expr->type;
  assert(type->kind == TY_FUNC ||
         (type->kind == TY_PTR && type->pa.ptrof->kind == TY_FUNC));
  if (expr->kind == EX_VAR) {
    bool global = false;
    if (is_global_scope(expr->var.scope)) {
      global = true;
    } else {
      Scope *scope;
      VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
      global = (varinfo->storage & VS_EXTERN) != 0;
    }
    if (global && type->kind == TY_FUNC) {
      if (!table_try_get(&builtin_function_table, expr->var.name, NULL))
        register_func_info(expr->var.name, NULL, type, FF_REFERED);
    } else {
      assert(type->kind == TY_PTR && type->pa.ptrof->kind == TY_FUNC);
      getsert_func_type_index(type->pa.ptrof, true);
    }
  } else {
    traverse_expr(pexpr, true);
  }
}

static void traverse_funcall(Expr *expr) {
  Expr *func = expr->funcall.func;
  Type *functype = get_callee_type(func->type);
  if (functype == NULL) {
    parse_error(PE_NOFATAL, func->token, "Cannot call except function");
    return;
  }
  if (functype->func.ret->kind != TY_VOID && !is_prim_type(functype->func.ret)) {
    register_func_info_memcpy();
    // Allocate local variable for return value.
    assert(curfunc != NULL);
    assert(curfunc->scopes != NULL);
    assert(curfunc->scopes->len > 0);
    VarInfo *varinfo = add_var_to_scope(curfunc->scopes->data[0], alloc_dummy_ident(), functype->func.ret, 0);
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
          check_funcall_args(func, args, curscope, toplevel);
      }
    }

    if (functype->func.params == NULL && args != NULL)
      parse_error(PE_NOFATAL, func->token, "function's parameters must be known");
  }

  traverse_func_expr(&expr->funcall.func);
  if (args != NULL) {
    for (int i = 0, n = args->len; i < n; ++i) {
      Expr *arg = args->data[i];
      if (is_stack_param(arg->type))
        register_func_info_memcpy();
      traverse_expr((Expr**)&args->data[i], true);
    }
  }
}

static void traverse_expr(Expr **pexpr, bool needval) {
  Expr *expr = *pexpr;
  if (expr == NULL)
    return;
  switch (expr->kind) {
  //case EX_FIXNUM:  break;
  //case EX_FLONUM:  break;
  //case EX_STR:  break;
  case EX_VAR:
    if (expr->type->kind == TY_FUNC) {
      register_indirect_function(expr->var.name, expr->type);
      traverse_func_expr(pexpr);
    }
    break;

  // bop
  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
  case EX_DIV:
  case EX_MOD:
  case EX_BITAND:
  case EX_BITOR:
  case EX_BITXOR:
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_LE:
  case EX_GE:
  case EX_GT:
  case EX_LOGAND:
  case EX_LOGIOR:
    traverse_expr(&expr->bop.lhs, needval);
    traverse_expr(&expr->bop.rhs, needval);
    break;
  case EX_LSHIFT:
  case EX_RSHIFT:
    // Make sure that RHS type is same as LHS.
    expr->bop.rhs = make_cast(expr->bop.lhs->type, expr->bop.rhs->token, expr->bop.rhs, false);
    traverse_expr(&expr->bop.lhs, needval);
    traverse_expr(&expr->bop.rhs, needval);
    break;
  case EX_COMMA:
    traverse_expr(&expr->bop.lhs, false);
    traverse_expr(&expr->bop.rhs, needval);
    break;
  case EX_ASSIGN:
    traverse_expr(&expr->bop.lhs, true);
    traverse_expr(&expr->bop.rhs, true);
    expr->type = &tyVoid;
    if (!is_prim_type(expr->bop.lhs->type)) {
      register_func_info_memcpy();
    }
    if (needval) {
      Expr *lhs = expr->bop.lhs;
      if (!(lhs->kind == EX_VAR ||
            (lhs->kind == EX_DEREF && lhs->unary.sub->kind == EX_VAR))) {
        const Token *token = expr->token;
        Type *type = lhs->type, *ptrtype = ptrof(type);
        Expr *tmp = alloc_tmp(token, ptrtype);
        Expr *assign_tmp = new_expr_bop(
            EX_ASSIGN, &tyVoid, token, tmp,
            new_expr_unary(EX_REF, ptrtype, token, lhs));
        expr->bop.lhs = new_expr_unary(EX_DEREF, type, token, tmp);
        *pexpr = new_expr_bop(EX_COMMA, type, token, assign_tmp, expr);
        traverse_expr(pexpr, needval);
        return;
      }

      Expr *rhs = expr->bop.rhs;
      Type *type = rhs->type;
      if (!(is_const(rhs) || rhs->kind == EX_VAR)) {  // Rhs is not simple value.
        Expr *old = expr;
        expr = assign_to_tmp(expr, expr);
        rhs = old->bop.rhs;  // tmp
      }
      *pexpr = new_expr_bop(EX_COMMA, type, expr->token, expr, rhs);
    }
    break;

  // Unary
  case EX_POS:
  case EX_NEG:
  case EX_BITNOT:
  case EX_REF:
  case EX_DEREF:
  case EX_CAST:
    traverse_expr(&expr->unary.sub, needval);
    break;
  case EX_PREINC:
  case EX_PREDEC:
  case EX_POSTINC:
  case EX_POSTDEC:
    {
      static const enum ExprKind kOpAddSub[2] = {EX_ADD, EX_SUB};

      traverse_expr(&expr->unary.sub, needval);
      Expr *target = expr->unary.sub;
      if (target->kind == EX_COMPLIT)
        target = target->complit.var;
      if (target->kind == EX_VAR) {
        VarInfo *varinfo = scope_find(target->var.scope, target->var.name, NULL);
        if (!(varinfo->storage & VS_REF_TAKEN))
          break;
      }

      Type *type = target->type;
      assert(is_number(type) || type->kind == TY_PTR);
      bool post = expr->kind >= EX_POSTINC;
      bool dec = (expr->kind - EX_PREINC) & 1;
      // (++xxx)  =>  (p = &xxx, tmp = *p + 1, *p = tmp, tmp)
      // (xxx++)  =>  (p = &xxx, tmp = *p, *p = tmp + 1, tmp)
      const Token *token = target->token;
      Type *ptrtype = ptrof(type);
      Expr *p = alloc_tmp(token, ptrtype);
      Expr *tmp = alloc_tmp(token, type);
      enum ExprKind op = kOpAddSub[dec];

      Expr *assign_p = new_expr_bop(EX_ASSIGN, &tyVoid, token, p,
                                    new_expr_unary(EX_REF, ptrtype, token, target));
      Expr *deref_p = new_expr_deref(token, p);
      Expr *one = type->kind == TY_PTR ? new_expr_fixlit(&tySize, token, type_size(type->pa.ptrof))
          : new_expr_fixlit(type, token, 1);
      Expr *assign_tmp = new_expr_bop(EX_ASSIGN, &tyVoid, token, tmp,
                                      !post ? new_expr_bop(op, type, token, deref_p, one) : deref_p);
      Expr *assign_deref_p = new_expr_bop(EX_ASSIGN, &tyVoid, token, deref_p,
                                          !post ? tmp : new_expr_bop(op, type, token, tmp, one));

      *pexpr = new_expr_bop(
          EX_COMMA, type, token, assign_p,
          new_expr_bop(
              EX_COMMA, type, token, assign_tmp,
              new_expr_bop(
                  EX_COMMA, type, token, assign_deref_p, tmp)));
    }
    break;
  case EX_MODIFY:
    {
      traverse_expr(&expr->unary.sub, true);
      expr->type = &tyVoid;

      Expr *sub = expr->unary.sub;
      Expr *lhs = sub->bop.lhs;
      if (!(lhs->kind == EX_VAR ||
            (lhs->kind == EX_DEREF && lhs->unary.sub->kind == EX_VAR))) {
        const Token *token = sub->token;
        Type *type = lhs->type;
        Type *ptrtype = ptrof(type);
        Expr *tmp = alloc_tmp(token, ptrtype);
        Expr *assign_tmp = new_expr_bop(
            EX_ASSIGN, &tyVoid, token, tmp,
            new_expr_unary(EX_REF, ptrtype, token, lhs));
        sub->bop.lhs = new_expr_unary(EX_DEREF, type, token, tmp);
        *pexpr = new_expr_bop(EX_COMMA, type, token, assign_tmp, expr);
        traverse_expr(pexpr, needval);
        return;
      }

      if (needval)
        *pexpr = new_expr_bop(EX_COMMA, lhs->type, expr->token, expr, lhs);  // (lhs+=rhs, lhs)
    }
    break;

  case EX_TERNARY:
    traverse_expr(&expr->ternary.cond, true);
    traverse_expr(&expr->ternary.tval, needval);
    traverse_expr(&expr->ternary.fval, needval);
    break;

  case EX_MEMBER:
    traverse_expr(&expr->member.target, needval);
    break;

  case EX_FUNCALL:
    traverse_funcall(expr);
    break;

  case EX_COMPLIT:
    register_func_info_memset();
    traverse_stmts(expr->complit.inits);
    break;

  case EX_BLOCK:
    traverse_stmt(expr->block);
    break;

  default: break;
  }
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
      for (int i = 0; i < multi->len; ++i) {
        Initializer *elem = multi->data[i];
        if (elem == NULL)
          continue;
        switch (elem->kind) {
        case IK_SINGLE:
        case IK_MULTI:
          traverse_initializer(elem);
          break;
        case IK_DOT:
          traverse_initializer(elem->dot.value);
          break;
        case IK_ARR:
          traverse_initializer(elem->arr.value);
          break;
        }
      }
    }
    break;
  default: assert(false); break;
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
    const Name *name = alloc_label();
    Type *type = stmt->switch_.value->type;
    scope_add(scope, name, type, 0);

    // switch (complex)  =>  switch ((tmp = complex, tmp))
    Expr *var = new_expr_variable(name, type, NULL, scope);
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

static void traverse_vardecl(Stmt *stmt) {
  Vector *decls = stmt->vardecl.decls;
  if (decls != NULL) {
    for (int i = 0, n = decls->len; i < n; ++i) {
      VarDecl *decl = decls->data[i];
      if (decl->init == NULL)
        continue;
      traverse_initializer(decl->init);

      VarInfo *varinfo = scope_find(curscope, decl->ident->ident, NULL);
      if (varinfo != NULL && (varinfo->storage & (VS_STATIC | VS_EXTERN)) == 0 &&
          (varinfo->type->kind == TY_STRUCT ||
           varinfo->type->kind == TY_ARRAY)) {
        register_func_info_memset();
      }
    }
  }
  traverse_stmts(stmt->vardecl.inits);
}

static void traverse_stmt(Stmt *stmt) {
  if (stmt == NULL)
    return;
  switch (stmt->kind) {
  case ST_EXPR:  traverse_expr(&stmt->expr, false); break;
  case ST_RETURN:
    if (stmt->return_.val != NULL && !is_prim_type(stmt->return_.val->type))
      register_func_info_memcpy();
    traverse_expr(&stmt->return_.val, true);
    break;
  case ST_BLOCK:
    if (stmt->block.scope != NULL)
      curscope = stmt->block.scope;
    traverse_stmts(stmt->block.stmts);
    if (stmt->block.scope != NULL)
      curscope = curscope->parent;
    break;
  case ST_IF:  traverse_if(stmt); break;
  case ST_SWITCH:  traverse_switch(stmt); break;
  case ST_CASE: case ST_DEFAULT:  traverse_case(stmt); break;
  case ST_WHILE:  traverse_while(stmt); break;
  case ST_DO_WHILE:  traverse_do_while(stmt); break;
  case ST_FOR:  traverse_for(stmt); break;
  case ST_BREAK:  break;
  case ST_CONTINUE:  break;
  case ST_GOTO:
    parse_error(PE_FATAL, stmt->token, "cannot use goto");
    break;
  case ST_LABEL:  traverse_stmt(stmt->label.stmt); break;
  case ST_VARDECL:  traverse_vardecl(stmt); break;
  case ST_ASM:
    parse_error(PE_FATAL, stmt->token, "not supported");
    break;
  default:
    parse_error(PE_FATAL, stmt->token, "Unhandled stmt: %d", stmt->kind);
    break;
  }
}

static void traverse_stmts(Vector *stmts) {
  if (stmts == NULL)
    return;

  for (int i = 0, len = stmts->len; i < len; ++i) {
    Stmt *stmt = stmts->data[i];
    traverse_stmt(stmt);
  }
}

static void traverse_defun(Function *func) {
  if (func->scopes == NULL)  // Prototype definition
    return;

  func->extra = calloc(1, sizeof(FuncExtra));

  Type *functype = func->type;
  if (functype->func.params == NULL) {
    // Treat old-style function as a no-parameter function.
    Type *noparam = malloc(sizeof(*noparam));
    memcpy(noparam, functype, sizeof(*noparam));
    noparam->func.params = noparam->func.param_types = new_vector();
    VarInfo *varinfo = scope_find(global_scope, func->name, NULL);
    assert(varinfo != NULL);
    varinfo->type = func->type = functype = noparam;
  }
  if (functype->func.vaargs) {
    Type *tyvalist = find_typedef(curscope, alloc_name("__builtin_va_list", NULL, false), NULL);
    assert(tyvalist != NULL);

    const Name *name = alloc_name(VA_ARGS_NAME, NULL, false);
    scope_add(func->scopes->data[0], name, tyvalist, 0);
  }

  register_func_info(func->name, func, func->type, 0);
  curfunc = func;
  curscope = func->scopes->data[0];
  traverse_stmts(func->stmts);
  curscope = curscope->parent;
  curfunc = NULL;

  // Output static local variables.
  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    if (scope->vars == NULL)
      continue;
    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (!(varinfo->storage & VS_STATIC))
        continue;
      VarInfo *gvarinfo = varinfo->static_.gvar;
      assert(gvarinfo != NULL);
      register_gvar_info(gvarinfo->name, gvarinfo);
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
  case DCL_VARDECL:
    {
      Vector *decls = decl->vardecl.decls;
      for (int i = 0; i < decls->len; ++i) {
        VarDecl *d = decls->data[i];
        if (!(d->storage & VS_EXTERN)) {
          const Name *name = d->ident->ident;
          VarInfo *varinfo = scope_find(curscope, name, NULL);
          assert(varinfo != NULL);
          register_gvar_info(name, varinfo);
          traverse_initializer(d->init);
        }
      }
    }
    break;

  default:
    error("Unhandled decl: %d", decl->kind);
    break;
  }
}

static void add_builtins(void) {
  // Stack pointer.
  {
    const Name *name = alloc_name(SP_NAME, NULL, false);
    Type *type = &tySize;
    /*GVarInfo *info =*/ add_global_var(type, name);
  }
  {
    const Name *name = alloc_name(BREAK_ADDRESS_NAME, NULL, false);
    Type *type = &tySize;  // &tyVoidPtr
    /*GVarInfo *info =*/ add_global_var(type, name);
  }
  {
    const Name *name = alloc_name(MEMORY_PAGE_COUNT_NAME, NULL, false);
    Type *type = &tySize;
    /*GVarInfo *info =*/ add_global_var(type, name);
  }
}

uint32_t traverse_ast(Vector *decls, Vector *exports, uint32_t stack_size) {
  for (int i = 0, len = decls->len; i < len; ++i) {
    Declaration *decl = decls->data[i];
    traverse_decl(decl);
  }

  // Check exports
  for (int i = 0; i < exports->len; ++i) {
    const Name *name = exports->data[i];
    FuncInfo *info = table_get(&func_info_table, name);
    if (info == NULL)
      error("`%.*s' not found", name->bytes, name->chars);
    register_func_info(name, NULL, info->type, FF_REFERED);
  }

  {
    // Enumerate imported functions.
    VERBOSES("### Functions\n");
    const Name *name;
    FuncInfo *info;
    int32_t index = 0;
    for (int it = 0; (it = table_iterate(&func_info_table, it, &name, (void**)&info)) != -1; ) {
      if (info->func != NULL)
        continue;
      info->index = index++;
      VERBOSE("%2d: %.*s  (import)\n", info->index, name->bytes, name->chars);
    }
    // Enumerate defined and refered functions.
    for (int it = 0; (it = table_iterate(&func_info_table, it, &name, (void**)&info)) != -1; ) {
      if (info->func == NULL || info->flag == 0)
        continue;
      info->index = index++;
      VERBOSE("%2d: %.*s\n", info->index, name->bytes, name->chars);
    }
    VERBOSES("\n");
  }

  add_builtins();

  uint32_t sp_bottom;
  {
    // Enumerate global variables.
    const uint32_t START_ADDRESS = 1;  // Avoid valid poiter is NULL.
    uint32_t address = START_ADDRESS;
    const Name *name;
    GVarInfo *info;

    // Enumerate data and bss.
    VERBOSE("### Memory  0x%x\n", address);
    for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
      const VarInfo *varinfo = info->varinfo;
      if ((is_prim_type(varinfo->type) && !(varinfo->storage & VS_REF_TAKEN)) ||
          varinfo->global.init == NULL)
        continue;
      // Mapped to memory, with initializer
      address = ALIGN(address, align_size(varinfo->type));
      info->non_prim.address = address;
      size_t size = type_size(varinfo->type);
      address += size;
      VERBOSE("%04x: %.*s  (size=0x%lx)\n", info->non_prim.address, name->bytes, name->chars, size);
    }
    VERBOSE("---- BSS  0x%x\n", address);
    // Mapped to memory, without initialzer (BSS).
    for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
      const VarInfo *varinfo = info->varinfo;
      if ((is_prim_type(varinfo->type) && !(varinfo->storage & VS_REF_TAKEN)) ||
          varinfo->global.init != NULL)
        continue;
      address = ALIGN(address, align_size(varinfo->type));
      info->non_prim.address = address;
      size_t size = type_size(varinfo->type);
      address += size;
      VERBOSE("%04x: %.*s  (size=0x%lx)\n", info->non_prim.address, name->bytes, name->chars, size);
    }

    // Primitive types (Globals).
    VERBOSES("\n### Globals\n");
    uint32_t index = 0;
    for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
      const VarInfo *varinfo = info->varinfo;
      if (!is_prim_type(varinfo->type) || (varinfo->storage & VS_REF_TAKEN))
        continue;
      info->prim.index = index++;
      VERBOSE("%2d: %.*s\n", info->prim.index, name->bytes, name->chars);
    }
    VERBOSES("\n");

    // Set initial values.
    {  // Stack pointer.
      sp_bottom = ALIGN(address + stack_size, 16);
      GVarInfo *info = get_gvar_info_from_name(alloc_name(SP_NAME, NULL, false));
      assert(info != NULL);
      Initializer *init = calloc(1, sizeof(*init));
      init->kind = IK_SINGLE;
      init->single = new_expr_fixlit(info->varinfo->type, NULL, sp_bottom);
      info->varinfo->global.init = init;
      VERBOSE("SP bottom: 0x%x  (size=0x%x)\n", sp_bottom, stack_size);
    }
    {  // Break address.
      GVarInfo *info = get_gvar_info_from_name(alloc_name(BREAK_ADDRESS_NAME, NULL, false));
      assert(info != NULL);
      Initializer *init = calloc(1, sizeof(*init));
      init->kind = IK_SINGLE;
      init->single = new_expr_fixlit(info->varinfo->type, NULL, sp_bottom);
      info->varinfo->global.init = init;
      VERBOSE("Break address: 0x%x\n", sp_bottom);
    }
    {  // Memory page count.
      const uint32_t MEMORY_PAGE_BIT = 16;
      uint32_t page_count = (sp_bottom + ((1 << MEMORY_PAGE_BIT) - 1)) >> MEMORY_PAGE_BIT;
      assert(page_count > 0);

      GVarInfo *info = get_gvar_info_from_name(alloc_name(MEMORY_PAGE_COUNT_NAME, NULL, false));
      assert(info != NULL);
      Initializer *init = calloc(1, sizeof(*init));
      init->kind = IK_SINGLE;
      init->single = new_expr_fixlit(info->varinfo->type, NULL, page_count);
      info->varinfo->global.init = init;
      VERBOSE("Memory page count: 0x%x\n", page_count);
    }
  }

  return sp_bottom;
}
