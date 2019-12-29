#include "ast.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "type.h"
#include "util.h"

bool is_const(Expr *expr) {
  // TODO: Handle constant variable.

  switch (expr->kind) {
  case EX_NUM:
  case EX_STR:
    return true;
  default:
    return false;
  }
}

static Expr *new_expr(enum ExprKind kind, const Type *type, const Token *token) {
  Expr *expr = malloc(sizeof(*expr));
  expr->kind = kind;
  expr->type = type;
  expr->token = token;
  return expr;
}

Expr *new_expr_numlit(const Type *type, const Token *token, const Num *num) {
  assert(type->kind == TY_NUM);
  Expr *expr = new_expr(EX_NUM, type, token);
  expr->num = *num;
  return expr;
}

Expr *new_expr_str(const Token *token, const char *str, size_t size) {
  Type *type = malloc(sizeof(*type));
  type->kind = TY_ARRAY;
  type->pa.ptrof = &tyChar;
  type->pa.length = size;

  Expr *expr = new_expr(EX_STR, type, token);
  expr->str.buf = str;
  expr->str.size = size;
  return expr;
}

Expr *new_expr_varref(const char *name, const Type *type, const Token *token) {
  Expr *expr = new_expr(EX_VARREF, type, token);
  expr->varref.ident = name;
  expr->varref.scope = NULL;
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
  const Type *type = sub->type != NULL ? sub->type->pa.ptrof : NULL;
  return new_expr_unary(EX_DEREF, type, token, sub);
}

Expr *new_expr_ternary(const Token *token, Expr *cond, Expr *tval, Expr *fval, const Type *type) {
  Expr *expr = new_expr(EX_TERNARY, type, token);
  expr->ternary.cond = cond;
  expr->ternary.tval = tval;
  expr->ternary.fval = fval;
  return expr;
}

Expr *new_expr_member(const Token *token, const Type *type, Expr *target, const Token *acctok, const Token *ident, int index) {
  Expr *expr = new_expr(EX_MEMBER, type, token);
  expr->member.target = target;
  expr->member.acctok = acctok;
  expr->member.ident = ident;
  expr->member.index = index;
  return expr;
}

Expr *new_expr_funcall(const Token *token, Expr *func, Vector *args) {
  Expr *expr = new_expr(EX_FUNCALL, NULL, token);
  expr->funcall.func = func;
  expr->funcall.args = args;
  return expr;
}

Expr *new_expr_sizeof(const Token *token, const Type *type, Expr *sub) {
  Expr *expr = new_expr(EX_SIZEOF, &tySize, token);
  expr->sizeof_.type = type;
  expr->sizeof_.sub = sub;
  return expr;
}

Expr *new_expr_cast(const Type *type, const Token *token, Expr *sub) {
  Expr *expr = new_expr(EX_CAST, type, token);
  expr->unary.sub = sub;
  return expr;
}

// ================================================

VarDecl *new_vardecl(const Type *type, const Token *ident, Initializer *init, int flag) {
  VarDecl *decl = malloc(sizeof(*decl));
  decl->type = type;
  decl->ident = ident;
  decl->init = init;
  decl->flag = flag;
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

Stmt *new_stmt_block(const Token *token, Vector *stmts) {
  Stmt *stmt = new_stmt(ST_BLOCK, token);
  stmt->block.scope = NULL;
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
  stmt->switch_.case_values = new_vector();
  stmt->switch_.has_default = false;
  return stmt;
}

Stmt *new_stmt_case(const Token *token, Expr *value) {
  Stmt *stmt = new_stmt(ST_CASE, token);
  stmt->case_.value = value;
  return stmt;
}

Stmt *new_stmt_default(const Token *token) {
  Stmt *stmt = new_stmt(ST_DEFAULT, token);
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

Stmt *new_stmt_goto(const Token *label) {
  Stmt *stmt = new_stmt(ST_GOTO, NULL);
  stmt->goto_.label = label;
  return stmt;
}

Stmt *new_stmt_label(const Token *label, Stmt *follow) {
  Stmt *stmt = new_stmt(ST_LABEL, label);
  stmt->label.stmt = follow;
  return stmt;
}

Stmt *new_stmt_vardecl(Vector *decls) {
  Stmt *stmt = new_stmt(ST_VARDECL, NULL);
  stmt->vardecl.decls = decls;
  stmt->vardecl.inits = NULL;
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

Declaration *new_decl_defun(Defun *defun) {
  Declaration *decl = new_decl(DCL_DEFUN);
  decl->defun = defun;
  return decl;
}

Declaration *new_decl_vardecl(Vector *decls) {
  Declaration *decl = new_decl(DCL_VARDECL);
  decl->vardecl.decls = decls;
  return decl;
}
