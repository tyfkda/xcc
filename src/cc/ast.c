#include "ast.h"

#include <stdlib.h>  // malloc

#include "expr.h"
#include "util.h"

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

Stmt *new_stmt_defun(Defun *defun) {
  Stmt *stmt = new_stmt(ST_DEFUN, NULL);
  stmt->defun = defun;
  return stmt;
}

Stmt *new_top_stmt(Vector *stmts) {
  Stmt *top = new_stmt(ST_TOPLEVEL, NULL);
  top->toplevel.stmts = stmts;
  return top;
}
