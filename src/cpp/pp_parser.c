#include "pp_parser.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "lexer.h"
#include "table.h"
#include "type.h"
#include "util.h"

static PpExpr *parse_cast_expr(void);
static PpExpr *parse_unary(void);
PpExpr *parse_assign(void);

static PpExpr *new_expr(enum PpExprKind kind, const Token *token) {
  PpExpr *expr = malloc(sizeof(*expr));
  expr->kind = kind;
  //expr->type = type;
  expr->token = token;
  return expr;
}

PpExpr *new_expr_numlit(const Token *token, intptr_t num) {
  PpExpr *expr = new_expr(EX_NUM, token);
  expr->num = num;
  return expr;
}

PpExpr *new_expr_str(const Token *token, const char *str, size_t size) {
  Type *type = malloc(sizeof(*type));
  type->kind = TY_ARRAY;
  type->pa.ptrof = &tyChar;
  type->pa.length = size;

  PpExpr *expr = new_expr(EX_STR, token);
  expr->str.buf = str;
  expr->str.size = size;
  return expr;
}

PpExpr *new_expr_variable(const Name *name, const Token *token) {
  PpExpr *expr = new_expr(EX_VARIABLE, token);
  expr->variable.name = name;
  return expr;
}

PpExpr *new_expr_bop(enum PpExprKind kind, const Token *token, PpExpr *lhs, PpExpr *rhs) {
  PpExpr *expr = new_expr(kind, token);
  expr->bop.lhs = lhs;
  expr->bop.rhs = rhs;
  return expr;
}

PpExpr *new_expr_unary(enum PpExprKind kind, const Token *token, PpExpr *sub) {
  PpExpr *expr = new_expr(kind, token);
  expr->unary.sub = sub;
  return expr;
}

PpExpr *new_expr_funcall(const Token *token, PpExpr *func, Vector *args) {
  PpExpr *expr = new_expr(EX_FUNCALL, token);
  expr->funcall.func = func;
  expr->funcall.args = args;
  return expr;
}

//

Vector *parse_args(Token **ptoken) {
  Vector *args = NULL;
  Token *token;
  if ((token = match(TK_RPAR)) == NULL) {
    args = new_vector();
    for (;;) {
      PpExpr *arg = parse_assign();
      vec_push(args, arg);
      if ((token = match(TK_RPAR)) != NULL)
        break;
      consume(TK_COMMA, "Comma or `)` expected");
    }
  }

  *ptoken = token;
  return args;
}

static PpExpr *parse_funcall(PpExpr *func) {
  Token *token;
  Vector *args = parse_args(&token);
  return new_expr_funcall(token, func, args);
}

static PpExpr *parse_prim(void) {
  Token *tok;
  if ((tok = match(TK_LPAR)) != NULL) {
    PpExpr *expr = parse_expr();
    consume(TK_RPAR, "No close paren");
    return expr;
  }

  if ((tok = match(TK_CHARLIT)) != NULL ||
      (tok = match(TK_INTLIT)) != NULL ||
      (tok = match(TK_LONGLIT)) != NULL ||
      (tok = match(TK_UCHARLIT)) != NULL ||
      (tok = match(TK_UINTLIT)) != NULL ||
      (tok = match(TK_ULONGLIT)) != NULL) {
    return new_expr_numlit(tok, tok->value);
  }
  if ((tok = match(TK_STR)) != NULL)
    return new_expr_str(tok, tok->str.buf, tok->str.size);

  Token *ident = consume(TK_IDENT, "Number or Ident or open paren expected");
  const Name *name = ident->ident;
  return new_expr_variable(name, ident);
}

static PpExpr *parse_postfix(void) {
  PpExpr *expr = parse_prim();

  for (;;) {
    //Token *tok;
    if (match(TK_LPAR))
      expr = parse_funcall(expr);
    //else if ((tok = match(TK_LBRACKET)) != NULL)
    //  expr = parse_array_index(tok, expr);
    //else if ((tok = match(TK_INC)) != NULL)
    //  expr = new_expr_unary(EX_POSTINC, NULL, tok, expr);
    //else if ((tok = match(TK_DEC)) != NULL)
    //  expr = new_expr_unary(EX_POSTDEC, NULL, tok, expr);
    else
      return expr;
  }
}

static PpExpr *parse_unary(void) {
  Token *tok;
  if ((tok = match(TK_ADD)) != NULL) {
    PpExpr *expr = parse_cast_expr();
    switch (expr->kind) {
    case EX_NUM:
      return expr;
    default:
      return new_expr_unary(EX_POS, tok, expr);
    }

    return expr;
  }

  if ((tok = match(TK_SUB)) != NULL) {
    PpExpr *expr = parse_cast_expr();
    switch (expr->kind) {
    case EX_NUM:
      expr->num = -expr->num;
      return expr;
    default:
      return new_expr_unary(EX_NEG, tok, expr);
    }
  }

  if ((tok = match(TK_NOT)) != NULL) {
    PpExpr *expr = parse_cast_expr();
    return new_expr_unary(EX_NOT, tok, expr);
  }

  if ((tok = match(TK_TILDA)) != NULL) {
    PpExpr *expr = parse_cast_expr();
    return new_expr_unary(EX_BITNOT, tok, expr);
  }

  //if ((tok = match(TK_AND)) != NULL) {
  //  PpExpr *expr = parse_cast_expr();
  //  return new_expr_unary(EX_REF, NULL, tok, expr);
  //}

  //if ((tok = match(TK_MUL)) != NULL) {
  //  PpExpr *expr = parse_cast_expr();
  //  return new_expr_unary(EX_DEREF, NULL, tok, expr);
  //}

  //if ((tok = match(TK_INC)) != NULL) {
  //  PpExpr *expr = parse_unary();
  //  return new_expr_unary(EX_PREINC, NULL, tok, expr);
  //}

  //if ((tok = match(TK_DEC)) != NULL) {
  //  PpExpr *expr = parse_unary();
  //  return new_expr_unary(EX_PREDEC, NULL, tok, expr);
  //}

  return parse_postfix();
}

static PpExpr *parse_cast_expr(void) {
  return parse_unary();
}

static PpExpr *parse_mul(void) {
  PpExpr *expr = parse_cast_expr();

  for (;;) {
    enum PpExprKind kind;
    Token *tok;
    if ((tok = match(TK_MUL)) != NULL)
      kind = EX_MUL;
    else if ((tok = match(TK_DIV)) != NULL)
      kind = EX_DIV;
    else if ((tok = match(TK_MOD)) != NULL)
      kind = EX_MOD;
    else
      return expr;

    expr = new_expr_bop(kind, tok, expr, parse_cast_expr());
  }
}

static PpExpr *parse_add(void) {
  PpExpr *expr = parse_mul();

  for (;;) {
    enum PpExprKind t;
    Token *tok;
    if ((tok = match(TK_ADD)) != NULL)
      t = EX_ADD;
    else if ((tok = match(TK_SUB)) != NULL)
      t = EX_SUB;
    else
      return expr;

    expr = new_expr_bop(t, tok, expr, parse_mul());
  }
}

static PpExpr *parse_shift(void) {
  PpExpr *expr = parse_add();

  for (;;) {
    enum PpExprKind t;
    Token *tok;
    if ((tok = match(TK_LSHIFT)) != NULL)
      t = EX_LSHIFT;
    else if ((tok = match(TK_RSHIFT)) != NULL)
      t = EX_RSHIFT;
    else
      return expr;

    PpExpr *lhs = expr, *rhs = parse_add();
    expr = new_expr_bop(t, tok, lhs, rhs);
  }
}

static PpExpr *parse_cmp(void) {
  PpExpr *expr = parse_shift();

  for (;;) {
    enum PpExprKind t;
    Token *tok;
    if ((tok = match(TK_LT)) != NULL)
      t = EX_LT;
    else if ((tok = match(TK_GT)) != NULL)
      t = EX_GT;
    else if ((tok = match(TK_LE)) != NULL)
      t = EX_LE;
    else if ((tok = match(TK_GE)) != NULL)
      t = EX_GE;
    else
      return expr;

    PpExpr *lhs = expr, *rhs = parse_shift();
    expr = new_expr_bop(t, tok, lhs, rhs);
  }
}

static PpExpr *parse_eq(void) {
  PpExpr *expr = parse_cmp();

  for (;;) {
    enum PpExprKind t;
    Token *tok;
    if ((tok = match(TK_EQ)) != NULL)
      t = EX_EQ;
    else if ((tok = match(TK_NE)) != NULL)
      t = EX_NE;
    else
      return expr;

    PpExpr *lhs = expr, *rhs = parse_cmp();
    expr = new_expr_bop(t, tok, lhs, rhs);
  }
}

static PpExpr *parse_and(void) {
  PpExpr *expr = parse_eq();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_AND)) != NULL) {
      PpExpr *lhs = expr, *rhs = parse_eq();
      expr = new_expr_bop(EX_BITAND, tok, lhs, rhs);
    } else
      return expr;
  }
}

static PpExpr *parse_xor(void) {
  PpExpr *expr = parse_and();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_HAT)) != NULL) {
      PpExpr *lhs = expr, *rhs= parse_and();
      expr = new_expr_bop(EX_BITXOR, tok, lhs, rhs);
    } else
      return expr;
  }
}

static PpExpr *parse_or(void) {
  PpExpr *expr = parse_xor();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_OR)) != NULL) {
      PpExpr *lhs = expr, *rhs = parse_xor();
      expr = new_expr_bop(EX_BITOR, tok, lhs, rhs);
    } else
      return expr;
  }
}

static PpExpr *parse_logand(void) {
  PpExpr *expr = parse_or();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_LOGAND)) != NULL)
      expr = new_expr_bop(EX_LOGAND, tok, expr, parse_or());
    else
      return expr;
  }
}

static PpExpr *parse_logior(void) {
  PpExpr *expr = parse_logand();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_LOGIOR)) != NULL)
      expr = new_expr_bop(EX_LOGIOR, tok, expr, parse_logand());
    else
      return expr;
  }
}

static PpExpr *parse_conditional(void) {
  return parse_logior();
}

PpExpr *parse_assign(void) {
  return parse_conditional();
}

PpExpr *parse_const(void) {
  return parse_conditional();
}

PpExpr *parse_expr(void) {
  PpExpr *expr = parse_assign();
  const Token *tok;
  while ((tok = match(TK_COMMA)) != NULL) {
    PpExpr *next_expr = parse_assign();
    expr = new_expr_bop(EX_COMMA, tok, expr, next_expr);
  }
  return expr;
}
