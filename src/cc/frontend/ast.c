#include "../../config.h"
#include "ast.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "type.h"
#include "util.h"

Token *alloc_token(enum TokenKind kind, Line *line, const char *begin, const char *end) {
  if (end == NULL) {
    assert(begin != NULL);
    end = begin + strlen(begin);
  }
  Token *token = malloc_or_die(sizeof(*token));
  token->kind = kind;
  token->line = line;
  token->begin = begin;
  token->end = end;
  return token;
}

Token *alloc_ident(const Name *name, Line *line, const char *begin, const char *end) {
  Token *tok = alloc_token(TK_IDENT, line, begin, end);
  tok->ident = name;
  return tok;
}

bool is_const(Expr *expr) {
  if (expr->type->qualifier & TQ_VOLATILE)
    return false;

  switch (expr->kind) {
  case EX_FIXNUM:
  case EX_FLONUM:
  case EX_STR:
    return true;
  default:
    return false;
  }
}

bool is_const_truthy(Expr *expr) {
  // TODO: Handle constant variable.

  switch (expr->kind) {
  case EX_FIXNUM: return expr->fixnum != 0;
#ifndef __NO_FLONUM
  case EX_FLONUM: return expr->flonum != 0;
#endif
  case EX_STR:
    return true;
  case EX_VAR:
    return expr->type->kind == TY_ARRAY;
  default:
    return false;
  }
}

bool is_const_falsy(Expr *expr) {
  switch (expr->kind) {
  case EX_FIXNUM:  return expr->fixnum == 0;
#ifndef __NO_FLONUM
  case EX_FLONUM:  return expr->flonum == 0;
#endif
  default:
    return false;
  }
}

bool is_zero(Expr *expr) {
  return expr->kind == EX_FIXNUM && expr->fixnum == 0;
}

Expr *strip_cast(Expr *expr) {
  while (expr->kind == EX_CAST)
    expr = expr->unary.sub;
  return expr;
}

Expr *new_expr(enum ExprKind kind, Type *type, const Token *token) {
  Expr *expr = calloc_or_die(sizeof(*expr));
  expr->kind = kind;
  expr->type = type;
  expr->token = token;
  return expr;
}

Expr *new_expr_fixlit(Type *type, const Token *token, Fixnum fixnum) {
  assert(type->kind == TY_FIXNUM || type->kind == TY_PTR);
  if (type->fixnum.kind == FX_BOOL)
    fixnum = fixnum != 0;
  Expr *expr = new_expr(EX_FIXNUM, type, token);
  expr->fixnum = fixnum;
  return expr;
}

#ifndef __NO_FLONUM
Expr *new_expr_flolit(Type *type, const Token *token, Flonum flonum) {
  assert(type->kind == TY_FLONUM);
  Expr *expr = new_expr(EX_FLONUM, type, token);
  expr->flonum = flonum;
  return expr;
}
#endif

Expr *new_expr_variable(const Name *name, Type *type, const Token *token, Scope *scope) {
  Expr *expr = new_expr(EX_VAR, type, token);
  expr->var.name = name;
  expr->var.scope = scope;
  return expr;
}

Expr *new_expr_bop(enum ExprKind kind, Type *type, const Token *token, Expr *lhs, Expr *rhs) {
  Expr *expr = new_expr(kind, type, token);
  expr->bop.lhs = lhs;
  expr->bop.rhs = rhs;
  return expr;
}

Expr *new_expr_unary(enum ExprKind kind, Type *type, const Token *token, Expr *sub) {
  Expr *expr = new_expr(kind, type, token);
  expr->unary.sub = sub;
  return expr;
}

Expr *new_expr_deref(const Token *token, Expr *sub) {
  assert(sub->type != NULL);
  assert(sub->type->kind == TY_PTR || sub->type->kind == TY_ARRAY);
  Type *type = sub->type->pa.ptrof;
  return new_expr_unary(EX_DEREF, type, token, sub);
}

Expr *new_expr_ternary(const Token *token, Expr *cond, Expr *tval, Expr *fval, Type *type) {
  Expr *expr = new_expr(EX_TERNARY, type, token);
  expr->ternary.cond = cond;
  expr->ternary.tval = tval;
  expr->ternary.fval = fval;
  return expr;
}

Expr *new_expr_member(const Token *token, Type *type, Expr *target, const Name *ident,
                      const MemberInfo *minfo) {
  Expr *expr = new_expr(EX_MEMBER, type, token);
  expr->member.target = target;
  expr->member.ident = ident;
  expr->member.info = minfo;
  return expr;
}

Expr *new_expr_funcall(const Token *token, const Type *functype, Expr *func, Vector *args) {
  Expr *expr = new_expr(EX_FUNCALL, functype->func.ret, token);
  expr->funcall.func = func;
  expr->funcall.args = args;
  expr->funcall.fcinfo = NULL;
  return expr;
}

Expr *new_expr_inlined(const Token *token, const Name *name, Type *rettype, Vector *args,
                       Stmt *embedded) {
  Expr *expr = new_expr(EX_INLINED, rettype, token);
  expr->inlined.funcname = name;
  expr->inlined.args = args;
  expr->inlined.embedded = embedded;
  return expr;
}

Expr *new_expr_cast(Type *type, const Token *token, Expr *sub) {
  Expr *expr = new_expr(EX_CAST, type, token);
  expr->unary.sub = sub;
  return expr;
}

Expr *new_expr_complit(Type *type, const Token *token, Expr *var, Vector *inits,
                       Initializer *original) {
  Expr *expr = new_expr(EX_COMPLIT, type, token);
  expr->complit.var = var;
  expr->complit.inits = inits;
  expr->complit.original_init = original;
  return expr;
}

Expr *new_expr_block(Stmt *block) {
  static Type tyVoid = {.kind=TY_VOID};
  assert(block->kind == ST_BLOCK);
  Type *type = &tyVoid;
  Vector *stmts = block->block.stmts;
  if (stmts->len > 0) {
    Stmt *last = stmts->data[stmts->len - 1];
    if (last->kind == ST_EXPR)
      type = last->expr->type;
  }
  Expr *expr = new_expr(EX_BLOCK, type, block->token);
  expr->block = block;
  return expr;
}

// ================================================

Initializer *new_initializer(enum InitializerKind kind, const Token *token) {
  Initializer *init = calloc_or_die(sizeof(*init));
  init->kind = kind;
  init->token = token;
  return init;
}

VarDecl *new_vardecl(VarInfo *varinfo) {
  VarDecl *decl = malloc_or_die(sizeof(*decl));
  decl->varinfo = varinfo;
  decl->init_stmt = NULL;
  return decl;
}

Stmt *new_stmt(enum StmtKind kind, const Token *token) {
  Stmt *stmt = malloc_or_die(sizeof(Stmt));
  stmt->kind = kind;
  stmt->token = token;
  stmt->reach = 0;
  return stmt;
}

Stmt *new_stmt_expr(Expr *e) {
  Stmt *stmt = new_stmt(ST_EXPR, e->token);
  stmt->expr = e;
  return stmt;
}

Stmt *new_stmt_block(const Token *token, Vector *stmts, Scope *scope, const Token *rbrace) {
  Stmt *stmt = new_stmt(ST_BLOCK, token);
  stmt->block.scope = scope;
  stmt->block.stmts = stmts;
  stmt->block.rbrace = rbrace;
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

Stmt *new_stmt_case(const Token *token, Stmt *swtch, Expr *value) {
  Stmt *stmt = new_stmt(ST_CASE, token);
  stmt->case_.swtch = swtch;
  stmt->case_.value = value;
  stmt->case_.stmt = NULL;
  stmt->case_.bb = NULL;
  return stmt;
}

// Make sure inline function is out.
extern Stmt *new_stmt_default(const Token *token, Stmt *swtch);

Stmt *new_stmt_while(const Token *token, Expr *cond, Stmt *body) {
  Stmt *stmt = new_stmt(ST_WHILE, token);
  stmt->while_.cond = cond;
  stmt->while_.body = body;
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
  stmt->label.bb = NULL;
  return stmt;
}

Stmt *new_stmt_vardecl(VarDecl *vardecl) {
  Stmt *stmt = new_stmt(ST_VARDECL, NULL);
  stmt->vardecl = vardecl;
  return stmt;
}

Stmt *new_stmt_asm(const Token *token, Vector *templates, Vector *outputs, Vector *inputs,
                   int flag) {
  Stmt *stmt = new_stmt(ST_ASM, token);
  stmt->asm_.templates = templates;
  stmt->asm_.outputs = outputs;
  stmt->asm_.inputs = inputs;
  stmt->asm_.flag = flag;
  return stmt;
}

//

static Declaration *new_decl(enum DeclKind kind) {
  Declaration *decl = malloc_or_die(sizeof(*decl));
  decl->kind = kind;
  return decl;
}

Declaration *new_decl_defun(Function *func) {
  Declaration *decl = new_decl(DCL_DEFUN);
  decl->defun.func = func;
  return decl;
}

Declaration *new_decl_asm(const Token *token, Asm *asm_) {
  Declaration *decl = new_decl(DCL_ASM);
  decl->asm_.token = token;
  decl->asm_.asm_ = asm_;
  return decl;
}

// ================================================

// Function

Function *new_func(Type *type, const Token *ident, const Vector *params, Table *attributes,
                   int flag) {
  assert(type->kind == TY_FUNC);
  assert(ident->kind == TK_IDENT);
  Function *func = calloc_or_die(sizeof(*func));
  func->type = type;
  func->ident = ident;
  func->params = params;

  func->static_vars = NULL;
  func->scopes = NULL;
  func->body_block = NULL;
  func->label_table = NULL;
  func->extra = NULL;
  func->attributes = attributes;
  func->flag = flag;

  return func;
}
