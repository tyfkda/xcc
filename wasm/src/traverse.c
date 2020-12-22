#include "wcc.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "ast.h"
#include "lexer.h"  // alloc_ident, parse_error
#include "parser.h"  // curscope
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

const char RETVAL_NAME[] = ".._RETVAL";

Table func_info_table;
Table gvar_info_table;

bool is_prim_type(const Type *type) {
  return is_number(type);
}

static void register_func_info(const Name *funcname, Function *func, const Type *type, int flag) {
  FuncInfo *info;
  if (!table_try_get(&func_info_table, funcname, (void**)&info)) {
    info = calloc(1, sizeof(*info));
    table_put(&func_info_table, funcname, info);
  }
  if (func != NULL)
    info->func = func;
  if (type != NULL)
    info->type = type;
  info->flag |= flag;
}

static GVarInfo *register_gvar_info(const Name *name, VarInfo *varinfo) {
#if !defined(NDEBUG)
  void *result;
  assert(!table_try_get(&gvar_info_table, name, &result));
#endif
  GVarInfo *info = calloc(1, sizeof(*info));
  info->varinfo = varinfo;
  table_put(&gvar_info_table, name, info);
  return info;
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
  if (varinfo == NULL ||
      (info = table_get(&gvar_info_table, varinfo->name)) == NULL) {
    parse_error(expr->token, "Variable not found: %.*s", expr->var.name->bytes, expr->var.name->chars);
  }
  return info;
}

static void traverse_stmts(Vector *stmts);

// l=r  =>  (t=r, l=t)
static Expr *assign_to_tmp(Expr *assign, Expr *bop) {
  const Token *token = assign->token;
  const Type *type = bop->bop.lhs->type;
  Expr *rhs = bop->bop.rhs;
  const Name *name = alloc_label();
  const Token *ident = alloc_ident(name, NULL, NULL);
  scope_add(curscope, ident, type, 0);
  Expr *tmp = new_expr_variable(name, type, token, curscope);
  Expr *assign_tmp = new_expr_bop(EX_ASSIGN, &tyVoid, token, tmp, rhs);
  bop->bop.rhs = tmp;
  return new_expr_bop(EX_COMMA, type, token, assign_tmp, assign);
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
    {
      bool global = false;
      if (expr->type->kind == TY_FUNC) {  // For now, function only.
        if (is_global_scope(expr->var.scope)) {
          global = true;
        } else {
          Scope *scope;
          VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
          global = (varinfo->storage & VS_EXTERN) != 0;
        }
      }
      if (global)
        register_func_info(expr->var.name, NULL, expr->type, FF_REFERED);
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
  case EX_LSHIFT:
  case EX_RSHIFT:
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_LE:
  case EX_GE:
  case EX_GT:
  case EX_LOGAND:
  case EX_LOGIOR:
  case EX_PTRADD:
  case EX_PTRSUB:
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
    if (needval) {
      Expr *rhs = expr->bop.rhs;
      const Type *type = rhs->type;
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
  case EX_PREINC:
  case EX_PREDEC:
  case EX_POSTINC:
  case EX_POSTDEC:
  case EX_REF:
  case EX_DEREF:
  case EX_CAST:
    traverse_expr(&expr->unary.sub, needval);
    break;
  case EX_MODIFY:
    traverse_expr(&expr->unary.sub, true);
    expr->type = &tyVoid;
    if (needval) {
      Expr *sub = expr->unary.sub;
      Expr *rhs = sub->bop.rhs;
      const Type *type = rhs->type;
      if (!(is_const(rhs) || rhs->kind == EX_VAR)) {  // Rhs is not simple value.
        Expr *old = expr;
        expr = assign_to_tmp(expr, expr);
        rhs = old->unary.sub->bop.rhs;  // tmp
      }
      *pexpr = new_expr_bop(EX_COMMA, type, expr->token, expr, rhs);
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
    {
      traverse_expr(&expr->funcall.func, true);
      Vector *args = expr->funcall.args;
      if (args != NULL) {
        for (int i = 0, n = args->len; i < n; ++i)
          traverse_expr((Expr**)&args->data[i], true);
      }
    }
    break;

  case EX_COMPLIT:
    traverse_stmts(expr->complit.inits);
    break;

  default: break;
  }
}

static void traverse_stmt(Stmt *stmt) {
  if (stmt == NULL)
    return;
  switch (stmt->kind) {
  case ST_EXPR:  traverse_expr(&stmt->expr, false); break;
  case ST_RETURN:  traverse_expr(&stmt->return_.val, true); break;
  case ST_BLOCK:
    if (stmt->block.scope != NULL)
      curscope = stmt->block.scope;
    traverse_stmts(stmt->block.stmts);
    if (stmt->block.scope != NULL)
      curscope = curscope->parent;
    break;
  case ST_IF:  traverse_expr(&stmt->if_.cond, true); traverse_stmt(stmt->if_.tblock); traverse_stmt(stmt->if_.fblock); break;
  case ST_SWITCH:  traverse_expr(&stmt->switch_.value, true); traverse_stmt(stmt->switch_.body); break;
  //case ST_CASE:  break;
  //case ST_DEFAULT:  gen_default(); break;
  case ST_WHILE: case ST_DO_WHILE:  traverse_expr(&stmt->while_.cond, true); traverse_stmt(stmt->while_.body); break;
  case ST_FOR:  traverse_expr(&stmt->for_.pre, false); traverse_expr(&stmt->for_.cond, true); traverse_expr(&stmt->for_.post, false); traverse_stmt(stmt->for_.body); break;
  //case ST_BREAK:  gen_break(); break;
  //case ST_CONTINUE:  gen_continue(); break;
  //case ST_GOTO:  gen_goto(stmt); break;
  case ST_LABEL:  traverse_stmt(stmt->label.stmt); break;
  case ST_VARDECL:
    {
      Vector *decls = stmt->vardecl.decls;
      for (int i = 0, n = decls->len; i < n; ++i) {
        VarDecl *d = decls->data[i];
        if (d->storage & VS_STATIC) {
          VarInfo *varinfo = scope_find(curscope, d->ident->ident, NULL);
          assert(varinfo != NULL);
          VarInfo *g = varinfo->static_.gvar;
          register_gvar_info(g->name, g);
        }
      }
      traverse_stmts(stmt->vardecl.inits);
    }
    break;
  //case ST_ASM:  break;
  default: break;
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

  const Type *functype = func->type;
  if (functype->func.ret->kind != TY_VOID) {
    assert(is_number(functype->func.ret) || functype->func.ret->kind == TY_PTR);
    // Add local variable for return value.
    const Name *name = alloc_name(RETVAL_NAME, NULL, false);
    const Token *ident = alloc_ident(name, NULL, NULL);
    scope_add(func->scopes->data[0], ident, functype->func.ret, 0);
  }

  register_func_info(func->name, func, func->type, 0);
  curscope = func->scopes->data[0];
  traverse_stmts(func->stmts);
  curscope = curscope->parent;
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
        }
      }
    }
    break;

  default:
    error("Unhandled decl: %d", decl->kind);
    break;
  }
}

void traverse_ast(Vector *decls, Vector *exports) {
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

  if (gvar_info_table.count > 0) {
    // Enumerate global variables.
    VERBOSES("### Globals\n");
    uint32_t index = 0;
    uint32_t address = 1;  // Avoid valid poiter is NULL.
    const Name *name;
    GVarInfo *info;
    // Primitive types, or with initializer (DATA).
    for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
      const VarInfo *varinfo = info->varinfo;
      if (!is_prim_type(varinfo->type))
        continue;
      info->prim.index = index++;
      VERBOSE("%2d: %.*s\n", info->prim.index, name->bytes, name->chars);
    }
    // Primitive types, or with initializer (DATA).
    VERBOSES("\n### Memory\n");
    for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
      const VarInfo *varinfo = info->varinfo;
      if (is_prim_type(varinfo->type) || varinfo->global.init == NULL)
        continue;
      // Mapped to memory, with initializer
      address = ALIGN(address, align_size(varinfo->type));
      info->non_prim.address = address;
      address += type_size(varinfo->type);
      VERBOSE("%04x: %.*s\n", info->non_prim.address, name->bytes, name->chars);
    }
    VERBOSES("---- BSS\n");
    // Mapped to memory, without initialzer (BSS).
    for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
      const VarInfo *varinfo = info->varinfo;
      if (is_prim_type(varinfo->type) || varinfo->global.init != NULL)
        continue;
      address = ALIGN(address, align_size(varinfo->type));
      info->non_prim.address = address;
      address += type_size(varinfo->type);
      VERBOSE("%04x: %.*s\n", info->non_prim.address, name->bytes, name->chars);
    }
    VERBOSES("\n");
  }
}
