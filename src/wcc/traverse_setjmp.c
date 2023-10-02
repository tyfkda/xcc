#include "../config.h"
#include "wcc.h"

#include <assert.h>

#include "ast.h"
#include "fe_misc.h"  // curscope
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

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
  case EX_FIXNUM: case EX_FLONUM: case EX_STR: case EX_VAR:
    break;

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

  case EX_INLINED:
    traverse_ast_stmt(&expr->inlined.embedded, &lstack, param);
    break;

  case EX_COMPLIT:
    traverse_ast_stmts(expr->complit.inits, &lstack, param);
    break;

  case EX_BLOCK:
    traverse_ast_stmt(&expr->block, &lstack, param);
    break;
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
  case ST_CASE:
    return traverse_ast_expr(&stmt->case_.value, &lstack, param);
  case ST_GOTO:  break;
  case ST_LABEL:  return traverse_ast_stmt(&stmt->label.stmt, &lstack, param);
  case ST_VARDECL:
    {
      Vector *decls = stmt->vardecl.decls;
      if (decls != NULL) {
        for (int i = 0, n = decls->len; i < n; ++i) {
          VarDecl *decl = decls->data[i];
          // traverse_initializer(decl->init);
          if (traverse_ast_stmt(&decl->init_stmt, &lstack, param))
            return true;
        }
      }
    }
    break;
  case ST_ASM:  break;
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
  Expr *try_block_expr = new_expr_block(new_stmt_block(token, try_stmts, NULL, token));

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
      functype->func.ret, args);

  Expr *store_sp = new_expr_bop(EX_ASSIGN, &tyVoid, token,
                                new_expr_unary(EX_DEREF, &tySize, token, jmpbuf_env), get_sp_var());

  Vector *stmts = new_vector();
  vec_push(stmts, new_stmt_expr(store_sp));
  if (var != NULL) {
    vec_push(stmts, new_stmt_expr(new_expr_bop(EX_ASSIGN, &tyVoid, token, var,
                                               new_expr_fixlit(var->type, token, 0))));
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
            // Replace setjmp funcall assignment to dummy expression.
            *lstack->parent->pexpr = new_expr_fixlit(expr->type, expr->token, 0);
            return true;
          }
        } else if (e->kind == EX_EQ || e->kind == EX_NE) {
          // Int the case that the value of `setjmp' result directly.
          Expr *jmpbuf_env = expr->funcall.args->data[0];
          if (modify_if_setjmp(lstack->parent, jmpbuf_env, NULL)) {
            // Replace setjmp funcall comparison to dummy expression.
            *lstack->pexpr = new_expr_fixlit(expr->type, expr->token, 0);
            return true;
          }
        }
      }
      parse_error(PE_NOFATAL, expr->funcall.func->token, "Unhandled `setjmp' usage");
    }
  }
  return false;
}

void modify_ast_for_setjmp(int n) {
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
