#include "ast.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "type.h"
#include "util.h"

bool is_const(Expr *expr) {
  // TODO: Handle constant variable.

  switch (expr->kind) {
  case EX_FIXNUM:
#ifndef __NO_FLONUM
  case EX_FLONUM:
#endif
  case EX_STR:
    return true;
  default:
    return false;
  }
}

bool is_zero(Expr *expr) {
  return expr->kind == EX_FIXNUM && expr->fixnum == 0;
}

static Expr *new_expr(enum ExprKind kind, const Type *type, const Token *token) {
  Expr *expr = malloc(sizeof(*expr));
  expr->kind = kind;
  expr->type = type;
  expr->token = token;
  return expr;
}

Expr *new_expr_fixlit(const Type *type, const Token *token, const Fixnum fixnum) {
  assert(type->kind == TY_FIXNUM);
  Expr *expr = new_expr(EX_FIXNUM, type, token);
  expr->fixnum = fixnum;
  return expr;
}

#ifndef __NO_FLONUM
Expr *new_expr_flolit(const Type *type, const Token *token, double flonum) {
  assert(type->kind == TY_FLONUM);
  Expr *expr = new_expr(EX_FLONUM, type, token);
  expr->flonum = flonum;
  return expr;
}
#endif

Expr *new_expr_str(const Token *token, const char *str, size_t size) {
  Type *type = malloc(sizeof(*type));
  type->kind = TY_ARRAY;
  type->qualifier = TQ_CONST;
  type->pa.ptrof = &tyChar;
  type->pa.length = size;

  Expr *expr = new_expr(EX_STR, type, token);
  expr->str.buf = str;
  expr->str.size = size;
  return expr;
}

Expr *new_expr_variable(const Name *name, const Type *type, const Token *token, Scope *scope) {
  Expr *expr = new_expr(EX_VAR, type, token);
  expr->var.name = name;
  expr->var.scope = scope;
  return expr;
}

Expr *new_expr_bop(enum ExprKind kind, const Type *type, const Token *token, Expr *lhs, Expr *rhs) {
  Expr *expr = new_expr(kind, type, token);
  expr->bop.lhs = lhs;
  expr->bop.rhs = rhs;
  return expr;
}

Expr *new_expr_unary(enum ExprKind kind, const Type *type, const Token *token, Expr *sub) {
  Expr *expr = new_expr(kind, type, token);
  expr->unary.sub = sub;
  return expr;
}

Expr *new_expr_deref(const Token *token, Expr *sub) {
  assert(sub->type != NULL);
  assert(sub->type->kind == TY_PTR || sub->type->kind == TY_ARRAY);
  const Type *type = sub->type->pa.ptrof;
  return new_expr_unary(EX_DEREF, type, token, sub);
}

Expr *new_expr_ternary(const Token *token, Expr *cond, Expr *tval, Expr *fval, const Type *type) {
  Expr *expr = new_expr(EX_TERNARY, type, token);
  expr->ternary.cond = cond;
  expr->ternary.tval = tval;
  expr->ternary.fval = fval;
  return expr;
}

Expr *new_expr_member(const Token *token, const Type *type, Expr *target, const Token *ident,
                      int index) {
  Expr *expr = new_expr(EX_MEMBER, type, token);
  expr->member.target = target;
  expr->member.ident = ident;
  expr->member.index = index;
  return expr;
}

Expr *new_expr_funcall(const Token *token, Expr *func, const Type *functype, Vector *args) {
  Expr *expr = new_expr(EX_FUNCALL, functype->func.ret, token);
  expr->funcall.func = func;
  expr->funcall.args = args;
  return expr;
}

Expr *new_expr_cast(const Type *type, const Token *token, Expr *sub) {
  Expr *expr = new_expr(EX_CAST, type, token);
  expr->unary.sub = sub;
  return expr;
}

Expr *new_expr_complit(const Type *type, const Token *token, Expr *var, Vector *inits) {
  Expr *expr = new_expr(EX_COMPLIT, type, token);
  expr->complit.var = var;
  expr->complit.inits = inits;
  return expr;
}

// ================================================

VarDecl *new_vardecl(const Type *type, const Token *ident, Initializer *init, int storage) {
  VarDecl *decl = malloc(sizeof(*decl));
  decl->type = type;
  decl->ident = ident;
  decl->init = init;
  decl->storage = storage;
  return decl;
}

Stmt *new_stmt(enum StmtKind kind, const Token *token) {
  Stmt *stmt = malloc(sizeof(Stmt));
  stmt->kind = kind;
  stmt->token = token;
  return stmt;
}

Stmt *new_stmt_expr(Expr *e) {
  Stmt *stmt = new_stmt(ST_EXPR, e->token);
  stmt->expr = e;
  return stmt;
}

Stmt *new_stmt_block(const Token *token, Vector *stmts, Scope *scope) {
  Stmt *stmt = new_stmt(ST_BLOCK, token);
  stmt->block.scope = scope;
  stmt->block.stmts = stmts;
  return stmt;
}

Stmt *new_stmt_if(const Token *token, Expr *cond, Stmt *tblock, Stmt *fblock) {
  Stmt *stmt = new_stmt(ST_IF, token);
  stmt->if_.cond = cond;
  stmt->if_.tblock = tblock;
  stmt->if_.fblock = fblock;
  return stmt;
}

Stmt *new_stmt_switch(const Token *token, Expr *value) {
  Stmt *stmt = new_stmt(ST_SWITCH, token);
  stmt->switch_.value = value;
  stmt->switch_.body = NULL;
  stmt->switch_.cases = new_vector();
  stmt->switch_.default_ = NULL;
  stmt->switch_.break_bb = NULL;
  return stmt;
}

Stmt *new_stmt_case(const Token *token, Expr *value) {
  Stmt *stmt = new_stmt(ST_CASE, token);
  stmt->case_.value = value;
  stmt->case_.bb = NULL;
  return stmt;
}

Stmt *new_stmt_default(const Token *token) {
  Stmt *stmt = new_stmt(ST_DEFAULT, token);
  stmt->case_.value = NULL;
  stmt->case_.bb = NULL;
  return stmt;
}

Stmt *new_stmt_while(const Token *token, Expr *cond, Stmt *body) {
  Stmt *stmt = new_stmt(ST_WHILE, token);
  stmt->while_.cond = cond;
  stmt->while_.body = body;
  return stmt;
}

Stmt *new_stmt_do_while(Stmt *body, const Token *token, Expr *cond) {
  Stmt *stmt = new_stmt(ST_DO_WHILE, token);
  stmt->while_.body = body;
  stmt->while_.cond = cond;
  return stmt;
}

Stmt *new_stmt_for(const Token *token, Expr *pre, Expr *cond, Expr *post, Stmt *body) {
  Stmt *stmt = new_stmt(ST_FOR, token);
  stmt->for_.pre = pre;
  stmt->for_.cond = cond;
  stmt->for_.post = post;
  stmt->for_.body = body;
  return stmt;
}

Stmt *new_stmt_return(const Token *token, Expr *val) {
  Stmt *stmt = new_stmt(ST_RETURN, token);
  stmt->return_.val = val;
  return stmt;
}

Stmt *new_stmt_goto(const Token *tok, const Token *label) {
  Stmt *stmt = new_stmt(ST_GOTO, tok);
  stmt->goto_.label = label;
  return stmt;
}

Stmt *new_stmt_label(const Token *label, Stmt *follow) {
  Stmt *stmt = new_stmt(ST_LABEL, label);
  stmt->label.stmt = follow;
  return stmt;
}

Stmt *new_stmt_vardecl(Vector *decls, Vector *inits) {
  Stmt *stmt = new_stmt(ST_VARDECL, NULL);
  stmt->vardecl.decls = decls;
  stmt->vardecl.inits = inits;
  return stmt;
}

Stmt *new_stmt_asm(const Token *token, Expr *str) {
  Stmt *stmt = new_stmt(ST_ASM, token);
  stmt->asm_.str = str;
  return stmt;
}

//

static Declaration *new_decl(enum DeclKind kind) {
  Declaration *decl = malloc(sizeof(*decl));
  decl->kind = kind;
  return decl;
}

Declaration *new_decl_defun(Function *func) {
  Declaration *decl = new_decl(DCL_DEFUN);
  decl->defun.func = func;
  return decl;
}

Declaration *new_decl_vardecl(Vector *decls) {
  Declaration *decl = new_decl(DCL_VARDECL);
  decl->vardecl.decls = decls;
  return decl;
}

// ================================================

// Function

Function *new_func(const Type *type, const Name *name) {
  assert(type->kind == TY_FUNC);
  Function *func = malloc(sizeof(*func));
  func->type = type;
  func->name = name;

  func->scopes = NULL;
  func->stmts = NULL;
  func->label_table = NULL;
  func->gotos = NULL;

  func->ra = NULL;
  func->bbcon = NULL;
  func->ret_bb = NULL;
  func->retval = NULL;

  return func;
}
