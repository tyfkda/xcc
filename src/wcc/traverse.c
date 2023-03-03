#include "wcc.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>  // memcpy

#include "ast.h"
#include "lexer.h"
#include "limits.h"  // CHAR_BIT
#include "parser.h"  // curscope
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

const char SP_NAME[] = "__stack_pointer";  // Variable name for stack pointer (global).
const char BREAK_ADDRESS_NAME[] = "__curbrk";
const char VA_ARGS_NAME[] = ".._VA_ARGS";

Table func_info_table;
Table gvar_info_table;
Table builtin_function_table;
Vector *functypes;
Table indirect_function_table;

static Stmt *branching_stmt;

bool is_stack_param(const Type *type) {
  return !is_prim_type(type);
}

static WasmFuncType *wasm_func_type(const Type *type) {
  bool ret_param = type->func.ret->kind != TY_VOID && !is_prim_type(type->func.ret);
  const Vector *param_types = type->func.param_types;
  int param_count = 0;
  if (param_types != NULL) {
    for (int i = 0; i < param_types->len; ++i) {
      const Type *type = param_types->data[i];
      if (!is_stack_param(type))
        ++param_count;
    }
  }

  DataStorage d;
  data_init(&d);
  data_reserve(&d, 3 + param_count + 3);

  emit_uleb128(&d, -1, (int)ret_param + param_count + (type->func.vaargs ? 1 : 0));  // num params
  if (ret_param)
    data_push(&d, to_wtype(&tyVoidPtr));
  if (param_types != NULL) {
    for (int i = 0; i < param_types->len; ++i) {
      const Type *type = param_types->data[i];
      if (!is_stack_param(type))
        data_push(&d, to_wtype(type));
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

int getsert_func_type_index(const Type *type, bool reg) {
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

// static void register_func_info_if_not_exist(const Name *funcname, Type *(*callback)(void)) {
//   if (table_get(&func_info_table, funcname) == NULL) {
//     Type *functype = (*callback)();
//     register_func_info(funcname, NULL, functype, FF_REFERED);
//     scope_add(global_scope, funcname, functype, 0);
//   }
// }

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

typedef struct LexicalStack {
  struct LexicalStack *parent;
  Stmt **pstmt;
  Expr **pexpr;
} LexicalStack;

typedef struct TraverseAstParam TraverseAstParam;
typedef bool (*TraverseAstCallback)(LexicalStack *lstack, TraverseAstParam *param);

struct TraverseAstParam {
  TraverseAstCallback callback;
  void *userdata;
};

static bool traverse_ast_stmt(Stmt **pstmt, LexicalStack *parent, TraverseAstParam *param);
static bool traverse_ast_stmts(Vector *stmts, LexicalStack *parent, TraverseAstParam *param);

static bool traverse_func_ast(TraverseAstParam *param) {
  Scope *saved_scope = curscope;
  curscope = global_scope;
  bool res = traverse_ast_stmt(&curfunc->body_block, NULL, param);
  curscope = saved_scope;
  return res;
}

static bool traverse_ast_expr(Expr **pexpr, LexicalStack *parent, TraverseAstParam *param) {
  Expr *expr = *pexpr;
  if (expr == NULL)
    return false;

  LexicalStack lstack = {.parent = parent, .pexpr = pexpr};
  if (param->callback(&lstack, param))
    return true;

  switch (expr->kind) {
  // Bop
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
  case EX_LSHIFT:
  case EX_RSHIFT:
  case EX_COMMA:
  case EX_ASSIGN:
    return traverse_ast_expr(&expr->bop.lhs, &lstack, param) ||
           traverse_ast_expr(&expr->bop.rhs, &lstack, param);

  // Unary
  case EX_POS:
  case EX_NEG:
  case EX_BITNOT:
  case EX_REF:
  case EX_DEREF:
  case EX_CAST:
  case EX_PREINC:
  case EX_PREDEC:
  case EX_POSTINC:
  case EX_POSTDEC:
    return traverse_ast_expr(&expr->unary.sub, &lstack, param);

  case EX_TERNARY:
    return traverse_ast_expr(&expr->ternary.cond, &lstack, param) ||
           traverse_ast_expr(&expr->ternary.tval, &lstack, param) ||
           traverse_ast_expr(&expr->ternary.fval, &lstack, param);

  case EX_MEMBER:
    traverse_ast_expr(&expr->member.target, &lstack, param);
    break;

  case EX_FUNCALL:
    {
      if (traverse_ast_expr(&expr->funcall.func, &lstack, param))
        return true;
      Vector *args = expr->funcall.args;
      for (int i = 0; i < args->len; ++i) {
        if (traverse_ast_expr((Expr**)&args->data[i], &lstack, param))
          return true;
      }
    }
    break;

  case EX_COMPLIT:
    traverse_ast_stmts(expr->complit.inits, &lstack, param);
    break;

  case EX_BLOCK:
    traverse_ast_stmt(&expr->block, &lstack, param);
    break;

  default: break;
  }
  return false;
}

static bool traverse_ast_stmts(Vector *stmts, LexicalStack *lstack, TraverseAstParam *param) {
  if (stmts != NULL) {
    for (int i = 0, len = stmts->len; i < len; ++i) {
      if (traverse_ast_stmt((Stmt**)&stmts->data[i], lstack, param))
        return true;
    }
  }
  return false;
}

static bool traverse_ast_stmt(Stmt **pstmt, LexicalStack *parent, TraverseAstParam *param) {
  Stmt *stmt = *pstmt;
  if (stmt == NULL)
    return false;

  LexicalStack lstack = {.parent = parent, .pstmt = pstmt};
  if (param->callback(&lstack, param))
    return true;

  switch (stmt->kind) {
  case ST_EXPR:  return traverse_ast_expr(&stmt->expr, &lstack, param);
  case ST_BLOCK:
    {
      if (stmt->block.scope != NULL)
        curscope = stmt->block.scope;
      bool result = traverse_ast_stmts(stmt->block.stmts, &lstack, param);
      if (stmt->block.scope != NULL)
        curscope = curscope->parent;
      return result;
    }
  case ST_IF:
    return traverse_ast_expr(&stmt->if_.cond, &lstack, param) ||
           traverse_ast_stmt(&stmt->if_.tblock, &lstack, param) ||
           traverse_ast_stmt(&stmt->if_.fblock, &lstack, param);
  case ST_SWITCH:
    return traverse_ast_expr(&stmt->switch_.value, &lstack, param) ||
           traverse_ast_stmt(&stmt->switch_.body, &lstack, param);
  case ST_WHILE:
    return traverse_ast_expr(&stmt->while_.cond, &lstack, param) ||
           traverse_ast_stmt(&stmt->while_.body, &lstack, param);
  case ST_DO_WHILE:
    return traverse_ast_stmt(&stmt->while_.body, &lstack, param) ||
           traverse_ast_expr(&stmt->while_.cond, &lstack, param);
  case ST_FOR:
    return traverse_ast_expr(&stmt->for_.pre, &lstack, param) ||
           traverse_ast_expr(&stmt->for_.cond, &lstack, param) ||
           traverse_ast_expr(&stmt->for_.post, &lstack, param) ||
           traverse_ast_stmt(&stmt->for_.body, &lstack, param);
  case ST_BREAK:  break;
  case ST_CONTINUE:  break;
  case ST_RETURN:  return traverse_ast_expr(&stmt->return_.val, &lstack, param);
  case ST_CASE: case ST_DEFAULT:
    return traverse_ast_expr(&stmt->case_.value, &lstack, param);
  case ST_LABEL:  return traverse_ast_stmt(&stmt->label.stmt, &lstack, param);
  case ST_VARDECL:
    {
      Vector *decls = stmt->vardecl.decls;
      if (decls != NULL) {
        for (int i = 0, n = decls->len; i < n; ++i) {
          VarDecl *decl = decls->data[i];
          // traverse_initializer(decl->init);
          if (traverse_ast_stmt(&decl->init_stmt, &lstack, param))  return true;
        }
      }
    }
    break;
  case ST_ASM:  break;
  default:
    parse_error(PE_FATAL, stmt->token, "Unhandled stmt: %d", stmt->kind);
    break;
  }
  return false;
}

static bool modify_if_setjmp(LexicalStack *lp, Expr *jmpbuf_env, Expr *var) {
  // Assume `lp' points compare expression.
  assert(lp->pexpr != NULL);

  // Move upward while comma expression.
  for (; lp->parent->pexpr != NULL; lp = lp->parent) {
    Expr *e = *lp->parent->pexpr;
    if (e->kind != EX_COMMA || e->bop.rhs != *lp->pexpr)
      return false;
  }

  if (lp->parent->pstmt == NULL || (*lp->parent->pstmt)->kind != ST_IF)
    return false;
  Stmt *ifstmt = *lp->parent->pstmt;

  // Pass jmpbuf_env, var and try-block using `__builtin_try_catch_longjmp'
  // and generate code by built-in function (`gen_builtin_try_catch_longjmp').
  // `try-block' is passed as block-expression.

  Token *token = NULL;
  Vector *try_stmts = new_vector();
  vec_push(try_stmts, ifstmt);
  Expr *try_block_expr = new_expr_block(new_stmt_block(
      token, try_stmts, NULL, token));

  Vector *param_types = new_vector();
  vec_push(param_types, var != NULL ? var->type : &tyVoid);
  vec_push(param_types, try_block_expr->type);
  Type *functype = new_func_type(&tyVoid, NULL, param_types, false);

  Vector *args = new_vector();
  vec_push(args, jmpbuf_env);
  vec_push(args, var);
  vec_push(args, try_block_expr);

  Expr *try_catch_longjmp = new_expr_funcall(
      token, new_expr_variable(alloc_name("__builtin_try_catch_longjmp", NULL, false), functype, token, global_scope),
      functype, args);

  Expr *store_sp = new_expr_bop(EX_ASSIGN, &tyVoid, token, new_expr_unary(EX_DEREF, &tySize, token, jmpbuf_env), get_sp_var());

  Vector *stmts = new_vector();
  vec_push(stmts, new_stmt_expr(store_sp));
  if (var != NULL) {
    vec_push(stmts, new_stmt_expr(
        new_expr_bop(EX_ASSIGN, &tyVoid, token, var, new_expr_fixlit(var->type, token, 0))));
  }
  vec_push(stmts, new_stmt_expr(try_catch_longjmp));
  Stmt *modified = new_stmt_block(token, stmts, NULL, token);
  *lp->parent->pstmt = modified;
  return true;
}

typedef struct {
  Expr *var;
  Expr *jmpbuf_env;
} MafscbFindIfVar;
static bool mafscb_find_if_var(LexicalStack *lstack, TraverseAstParam *param) {
  if (lstack->pexpr != NULL) {
    LexicalStack *lp = lstack;
    Expr *expr = *lp->pexpr;
    MafscbFindIfVar *v = param->userdata;
    Expr *var = v->var;
    if (expr->kind == EX_VAR && equal_name(expr->var.name, var->var.name)) {
      Expr *e = *lp->parent->pexpr;
      if (e->kind == EX_COMMA && e->bop.rhs == expr) {
        lp = lp->parent;
        e = *lp->parent->pexpr;
      }
      if (e->kind == EX_EQ || e->kind == EX_NE)
        return modify_if_setjmp(lp->parent, v->jmpbuf_env, var);
    }
  }
  return false;
}

static bool mafscb_find_setjmp(LexicalStack *lstack, TraverseAstParam *param) {
  UNUSED(param);
  if (lstack->pexpr != NULL) {
    Expr *expr = *lstack->pexpr;
    if (expr->kind == EX_FUNCALL && expr->funcall.func->kind == EX_VAR &&
        equal_name(expr->funcall.func->var.name, alloc_name("__builtin_setjmp", NULL, false))) {
      if (lstack->parent->pexpr != NULL) {
        Expr *e = *lstack->parent->pexpr;
        if (e->kind == EX_ASSIGN && e->bop.lhs->kind == EX_VAR) {
          Expr *var = e->bop.lhs;
          const VarInfo *varinfo = scope_find(var->var.scope, var->var.name, NULL);
          assert(varinfo != NULL);
          if (varinfo->storage & VS_REF_TAKEN) {
            parse_error(PE_NOFATAL, var->token, "variable must not use with `&'");
            return true;
          }

          MafscbFindIfVar v = {
            .var = e->bop.lhs,
            .jmpbuf_env = expr->funcall.args->data[0],
          };
          TraverseAstParam param = {
            .callback = mafscb_find_if_var,
            .userdata = &v,
          };
          if (traverse_func_ast(&param)) {
            *lstack->parent->pexpr = new_expr_fixlit(expr->type, expr->token, 0);  // Replace setjmp funcall assignment to dummy expression.
            return true;
          }
        } else if (e->kind == EX_EQ || e->kind == EX_NE) {
          // Int the case that the value of `setjmp' result directly.
          Expr *jmpbuf_env = expr->funcall.args->data[0];
          if (modify_if_setjmp(lstack->parent, jmpbuf_env, NULL)) {
            *lstack->pexpr = new_expr_fixlit(expr->type, expr->token, 0);  // Replace setjmp funcall comparison to dummy expression.
            return true;
          }
        }
      }
      parse_error(PE_NOFATAL, expr->funcall.func->token, "Unhandled `setjmp' usage");
    }
  }
  return false;
}

static void modify_ast_for_setjmp(int n) {
  // Modify:
  //   int var = setjmp(jmpbuf);
  //   if (var == 0) { ... } else { ... }
  // to:
  //   int var = 0;
  //   for (;;) {
  //     __try({
  //       if (var == 0) { ... } else { ... }
  //       break;
  //     }), /*catch*/ ({
  //       var = param;
  //     });
  //   }

  TraverseAstParam param = {
    .callback = mafscb_find_setjmp,
  };
  for (int i = 0; i < n; ++i) {
    if (!traverse_func_ast(&param))
      break;
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

    if (functype->func.params == NULL)
      parse_error(PE_NOFATAL, func->token, "function's parameters must be known");
  }

  traverse_func_expr(&expr->funcall.func);
  for (int i = 0, n = args->len; i < n; ++i)
    traverse_expr((Expr**)&args->data[i], true);

  // setjmp?
  if (expr->funcall.func->kind == EX_VAR && is_global_scope(expr->funcall.func->var.scope) &&
      equal_name(expr->funcall.func->var.name, alloc_name("__builtin_setjmp", NULL, false))) {
    FuncExtra *extra = curfunc->extra;
    assert(extra != NULL);
    ++extra->setjmp_count;
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
    traverse_expr(&expr->bop.lhs, false);
    traverse_expr(&expr->bop.rhs, true);
    expr->type = &tyVoid;  // Make assigment expression as void.
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
    break;

  // Unary
  case EX_POS:
  case EX_NEG:
  case EX_BITNOT:
  case EX_REF:
  case EX_DEREF:
    traverse_expr(&expr->unary.sub, needval);
    break;
  case EX_CAST:
    traverse_expr(&expr->unary.sub, needval);
    {
      Expr *src = expr->unary.sub;
      Type *stype = src->type;
      Type *dtype = expr->type;
      bool du = dtype->kind != TY_FIXNUM || dtype->fixnum.is_unsigned;
      bool su = stype->kind != TY_FIXNUM || stype->fixnum.is_unsigned;
      if (su && !du) {  // signed <- unsigned.
        int d = type_size(dtype);
        int s = type_size(stype);
        if (d == s && d < I32_SIZE) {
          // This conversion requires conditional branch with MSB.
          // Stack machine architecture cannot handle the case well, so replace the expression.
          Expr *assign = NULL;
          if (src->kind != EX_VAR) {
            // To use the src value multiple times, store it to temporary variable.
            Expr *orgsrc = src;
            const Name *name = alloc_label();
            scope_add(curscope, name, stype, 0);
            Expr *var = new_expr_variable(name, stype, NULL, curscope);
            assign = new_expr_bop(EX_ASSIGN, &tyVoid, NULL, var, orgsrc);
            src = var;
          }
          // sign: src & (1 << (s * CHAR_BIT - 1))
          Expr *msb = new_expr_bop(EX_BITAND, stype, NULL, src,
              new_expr_fixlit(stype, NULL, 1 << (s * CHAR_BIT - 1)));
          // src | -msb
          Expr *replaced = new_expr_bop(EX_BITOR, dtype, NULL, src,
                  new_expr_unary(EX_NEG, stype, NULL, msb));
          *pexpr = assign == NULL ? replaced
              : new_expr_bop(EX_COMMA, dtype, NULL, assign, replaced);
          // traverse_expr(pexpr, needval);
        }
      }
    }
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
      Expr *p = alloc_tmp_var(curscope, ptrtype);
      Expr *tmp = alloc_tmp_var(curscope, type);
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
      for (int i = 0; i < multi->len; ++i)
        traverse_initializer(multi->data[i]);
    }
    break;
  case IK_DOT:
    traverse_initializer(init->dot.value);
    break;
  case IK_ARR:
    traverse_initializer(init->arr.value);
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
      traverse_initializer(decl->init);
      traverse_stmt(decl->init_stmt);
    }
  }
}

static void traverse_stmt(Stmt *stmt) {
  if (stmt == NULL)
    return;
  switch (stmt->kind) {
  case ST_EXPR:  traverse_expr(&stmt->expr, false); break;
  case ST_RETURN:
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
  case ST_ASM:  break;
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
  if (equal_name(func->name, alloc_name("main", NULL, false))) {
    // Force `main' function takes two arguments.
    if (functype->func.params->len < 1) {
      assert(func->scopes->len > 0);
      const Name *name = alloc_label();
      Type *type = &tyInt;
      Scope *scope = func->scopes->data[0];
      vec_push((Vector*)functype->func.params, scope_add(scope, name, type, 0));
      vec_push((Vector*)functype->func.param_types, type);
    }
    if (functype->func.params->len < 2) {
      assert(func->scopes->len > 0);
      Type *type = ptrof(ptrof(&tyChar));
      const Name *name = alloc_label();
      Scope *scope = func->scopes->data[0];
      vec_push((Vector*)functype->func.params, scope_add(scope, name, type, 0));
      vec_push((Vector*)functype->func.param_types, type);
    }
  }
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
  traverse_stmt(func->body_block);
  if (compile_error_count == 0) {
    int count = ((FuncExtra*)func->extra)->setjmp_count;
    if (count > 0)
      modify_ast_for_setjmp(count);
  }
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
}

uint32_t traverse_ast(Vector *decls, Vector *exports, uint32_t stack_size) {
  add_builtins();

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
      Initializer *init = new_initializer(IK_SINGLE, NULL);
      init->single = new_expr_fixlit(info->varinfo->type, NULL, sp_bottom);
      info->varinfo->global.init = init;
      VERBOSE("SP bottom: 0x%x  (size=0x%x)\n", sp_bottom, stack_size);
    }
    {  // Break address.
      GVarInfo *info = get_gvar_info_from_name(alloc_name(BREAK_ADDRESS_NAME, NULL, false));
      assert(info != NULL);
      Initializer *init = new_initializer(IK_SINGLE, NULL);
      init->single = new_expr_fixlit(info->varinfo->type, NULL, sp_bottom);
      info->varinfo->global.init = init;
      VERBOSE("Break address: 0x%x\n", sp_bottom);
    }
  }

  return sp_bottom;
}
