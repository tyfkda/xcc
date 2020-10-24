#include "pp_parser.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "lexer.h"
#include "table.h"
#include "type.h"
#include "util.h"

#include "macro.h"

extern Table macro_table;

static PpResult parse_prim(void);
static PpResult parse_cast_expr(void);

//

static PpResult expand_ident(const Token *ident) {
  Macro *macro = table_get(&macro_table, ident->ident);
  if (macro == NULL) {
    //parse_error(ident, "`%.s' not defined", ident->ident->bytes, ident->ident->chars);
    return 0;
  }

  Vector *args = NULL;
  if (macro->params != NULL)
    args = parse_funargs(NULL);

  StringBuffer sb;
  sb_init(&sb);
  expand(macro, ident, args, ident->ident, &sb);

  const char *left = get_lex_p();
  if (left != NULL)
    sb_append(&sb, left, NULL);
  char *expanded = sb_to_string(&sb);

  set_source_string(expanded, NULL, -1);

  return parse_prim();
}

static PpResult parse_defined(void) {
  bool lpar = match(TK_LPAR) != NULL;
  Token *ident = consume(TK_IDENT, "Ident expected");
  if (lpar)
    consume(TK_RPAR, "No close paren");

  void *dummy = 0;
  return table_try_get(&macro_table, ident->ident, &dummy) ? 1 : 0;
}

static PpResult parse_prim(void) {
  Token *tok;
  if ((tok = match(TK_LPAR)) != NULL) {
    PpResult result = parse_expr();
    consume(TK_RPAR, "No close paren");
    return result;
  }

  if ((tok = match(TK_CHARLIT)) != NULL ||
      (tok = match(TK_INTLIT)) != NULL ||
      (tok = match(TK_LONGLIT)) != NULL ||
      (tok = match(TK_UCHARLIT)) != NULL ||
      (tok = match(TK_UINTLIT)) != NULL ||
      (tok = match(TK_ULONGLIT)) != NULL) {
    return tok->value;
  }
  //if ((tok = match(TK_STR)) != NULL)
  //  return new_expr_str(tok, tok->str.buf, tok->str.size);

  Token *ident = consume(TK_IDENT, "Number or Ident or open paren expected");
  if (equal_name(ident->ident, alloc_name("defined", NULL, false))) {
    return parse_defined();
  } else {
    return expand_ident(ident);
  }
}

static PpResult parse_postfix(void) {
  PpResult result = parse_prim();

  //for (;;) {
    //Token *tok;
    //if (match(TK_LPAR))
    //  expr = parse_funcall(expr);
    //else if ((tok = match(TK_LBRACKET)) != NULL)
    //  expr = parse_array_index(tok, expr);
    //else if ((tok = match(TK_INC)) != NULL)
    //  expr = new_expr_unary(EX_POSTINC, NULL, tok, expr);
    //else if ((tok = match(TK_DEC)) != NULL)
    //  expr = new_expr_unary(EX_POSTDEC, NULL, tok, expr);
    //else
      return result;
  //}
}

static PpResult parse_unary(void) {
  Token *tok;
  if ((tok = match(TK_ADD)) != NULL) {
    return parse_cast_expr();
  }

  if ((tok = match(TK_SUB)) != NULL) {
    PpResult result = parse_cast_expr();
    return -result;
  }

  if ((tok = match(TK_NOT)) != NULL) {
    PpResult result = parse_cast_expr();
    return result ? 0 : 1;
  }

  if ((tok = match(TK_TILDA)) != NULL) {
    PpResult result = parse_cast_expr();
    return ~result;
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

static PpResult parse_cast_expr(void) {
  return parse_unary();
}

static PpResult parse_mul(void) {
  PpResult result = parse_cast_expr();
  for (;;) {
    Token *tok;
    if (!(((tok = match(TK_MUL)) != NULL) ||
          ((tok = match(TK_DIV)) != NULL) ||
          ((tok = match(TK_MOD)) != NULL)))
      return result;

    PpResult rhs = parse_cast_expr();
    switch (tok->kind) {
    case TK_MUL:  result *= rhs; break;
    case TK_DIV:  result /= rhs; break;
    case TK_MOD:  result %= rhs; break;
    default:  assert(false); break;
    }
  }
}

static PpResult parse_add(void) {
  PpResult result = parse_mul();
  for (;;) {
    Token *tok;
    if (!(((tok = match(TK_ADD)) != NULL) ||
          ((tok = match(TK_SUB)) != NULL)))
      return result;

    PpResult rhs = parse_mul();
    if (tok->kind == TK_ADD)
      result += rhs;
    else
      result -= rhs;
  }
}

static PpResult parse_shift(void) {
  PpResult result = parse_add();
  for (;;) {
    Token *tok;
    if (!(((tok = match(TK_LSHIFT)) != NULL) ||
          ((tok = match(TK_RSHIFT)) != NULL)))
      return result;

    PpResult lhs = result, rhs = parse_add();
    if (tok->kind == TK_LSHIFT)
      result = lhs << rhs;
    else
      result = lhs >> rhs;
  }
}

static PpResult parse_cmp(void) {
  PpResult result = parse_shift();
  for (;;) {
    Token *tok;
    if (!(((tok = match(TK_LT)) != NULL) ||
          ((tok = match(TK_GT)) != NULL) ||
          ((tok = match(TK_LE)) != NULL) ||
          ((tok = match(TK_GE)) != NULL)))
      return result;

    PpResult lhs = result, rhs = parse_shift();
    switch (tok->kind) {
    case TK_LT:  result = lhs <  rhs ? 1 : 0; break;
    case TK_LE:  result = lhs <= rhs ? 1 : 0; break;
    case TK_GE:  result = lhs >= rhs ? 1 : 0; break;
    case TK_GT:  result = lhs >  rhs ? 1 : 0; break;
    default:  assert(false); break;
    }
  }
}

static PpResult parse_eq(void) {
  PpResult result = parse_cmp();
  for (;;) {
    Token *tok;
    if (!(((tok = match(TK_EQ)) != NULL) ||
          ((tok = match(TK_NE)) != NULL)))
      return result;

    PpResult lhs = result, rhs = parse_cmp();
    result = lhs == rhs ? 1 : 0;
    if (tok->kind != TK_EQ)
      result = 1 - result;
  }
}

static PpResult parse_and(void) {
  PpResult result = parse_eq();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_AND)) != NULL) {
      PpResult lhs = result, rhs = parse_eq();
      result = lhs & rhs;
    } else
      return result;
  }
}

static PpResult parse_xor(void) {
  PpResult result = parse_and();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_HAT)) != NULL) {
      PpResult lhs = result, rhs= parse_and();
      result = lhs ^ rhs;
    } else
      return result;
  }
}

static PpResult parse_or(void) {
  PpResult result = parse_xor();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_OR)) != NULL) {
      PpResult lhs = result, rhs = parse_xor();
      result = lhs | rhs;
    } else
      return result;
  }
}

static PpResult parse_logand(void) {
  PpResult result = parse_or();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_LOGAND)) != NULL) {
      PpResult rhs = parse_logand();
      result = result && rhs;
    } else
      return result;
  }
}

static PpResult parse_logior(void) {
  PpResult result = parse_logand();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_LOGIOR)) != NULL) {
      PpResult rhs = parse_logand();
      result = result || rhs;
    } else
      return result;
  }
}

static PpResult parse_conditional(void) {
  return parse_logior();
}

static PpResult parse_assign(void) {
  return parse_conditional();
}

PpResult parse_expr(void) {
  PpResult result = parse_assign();
  const Token *tok;
  while ((tok = match(TK_COMMA)) != NULL) {
    PpResult next_result = parse_assign();
    result = next_result;
  }
  return result;
}

static Token *match2(enum TokenKind kind, Stream *stream) {
  Token *tok;
  for (;;) {
    tok = match(kind);
    if (tok == NULL || tok->kind != TK_EOF || stream == NULL)
      return tok;

    char *line = NULL;
    size_t capa = 0;
    ssize_t len = getline_(&line, &capa, stream->fp, 0);
    if (len == EOF)
      return tok;  // EOF
    ++stream->lineno;
    set_source_string(line, stream->filename, stream->lineno);
  }
}

Vector *parse_funargs(Stream *stream) {
  Vector *args = NULL;
  if (match2(TK_LPAR, stream)) {
    args = new_vector();
    if (!match2(TK_RPAR, stream)) {
      StringBuffer sb;
      sb_init(&sb);
      const char *start = NULL;
      const char *end = NULL;
      int paren = 0;
      for (;;) {
        Token *tok;
        for (;;) {
          tok = match(-1);
          if (tok->kind != TK_EOF)
            break;

          if (start != end)
            sb_append(&sb, start, end);
          if (!sb_empty(&sb))
            sb_append(&sb, "\n", NULL);
          start = end = NULL;

          ssize_t len = EOF;
          char *line = NULL;
          if (stream != NULL) {
            size_t capa = 0;
            len = getline_(&line, &capa, stream->fp, 0);
          }
          if (len == EOF) {
            parse_error(NULL, "`)' expected");
            return NULL;
          }
          ++stream->lineno;
          set_source_string(line, stream->filename, stream->lineno);
        }

        if (tok->kind == TK_COMMA || tok->kind == TK_RPAR) {
          if (paren <= 0) {
            if (sb_empty(&sb)) {
              if (start == end)
                parse_error(tok, "expression expected");
              vec_push(args, strndup_(start, end - start));
            } else {
              if (start != end)
                sb_append(&sb, start, end);
              vec_push(args, sb_to_string(&sb));
              sb_clear(&sb);
            }
            start = end = NULL;

            if (tok->kind == TK_RPAR)
              break;
            else
              continue;
          }

          if (tok->kind == TK_RPAR)
            --paren;
        } else if (tok->kind == TK_LPAR) {
          ++paren;
        }
        if (start == NULL)
          start = tok->begin;
        end = tok->end;
      }
    }
  }
  return args;
}
