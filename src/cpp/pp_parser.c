#include "../config.h"
#include "pp_parser.h"

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "lexer.h"
#include "macro.h"
#include "table.h"
#include "util.h"

static PpResult pp_prim(void);
static PpResult pp_cast_expr(void);

//

static Stream *pp_stream;

Stream *set_pp_stream(Stream *stream) {
  Stream *old = pp_stream;
  pp_stream = stream;
  return old;
}

static void pp_parse_error_valist(const Token *token, const char *fmt, va_list ap) {
  if (fmt != NULL) {
    if (token == NULL)
      token = fetch_token();
    if (token != NULL && token->line != NULL) {
      fprintf(stderr, "%s(%d): ", token->line->filename, token->line->lineno);
    }

    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
  }

  if (token != NULL && token->line != NULL && token->begin != NULL)
    show_error_line(token->line->buf, token->begin, token->end - token->begin);
}

void pp_parse_error(const Token *token, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  pp_parse_error_valist(token, fmt, ap);
  va_end(ap);

  exit(1);
}

//

extern Lexer lexer;

typedef struct {
  Lexer lexer;
  LexEofCallback parent_callback;
  const Name *ident;
  void (*user_callback)(void);
} LexStackElem;

typedef struct {
  LexStackElem *buf;
  int capacity, sp;
} LexStack;

static LexStack lex_stack;
static Table macro_hideset;

Macro *can_expand_ident(const Name *ident) {
  return table_try_get(&macro_hideset, ident, NULL) ? NULL : macro_get(ident);
}

static bool on_process_line_eof(void) {
  assert(lex_stack.sp > 0);
  LexStackElem *p = &lex_stack.buf[--lex_stack.sp];
  lexer = p->lexer;
  set_lex_eof_callback(p->parent_callback);
  table_delete(&macro_hideset, p->ident);

  if (p->user_callback != NULL) {
    (*p->user_callback)();
  }
  return true;
}

void push_lex(const Name *ident, void (*callback)(void)) {
  if (lex_stack.sp >= lex_stack.capacity) {
    lex_stack.capacity += 1;
    LexStackElem *new_stack = realloc(lex_stack.buf, sizeof(*new_stack) * lex_stack.capacity);
    if (new_stack == NULL)
      error("out of memory");
    lex_stack.buf = new_stack;
  }

  LexStackElem *p = &lex_stack.buf[lex_stack.sp++];
  p->lexer = lexer;
  p->ident = ident;
  p->parent_callback = set_lex_eof_callback(on_process_line_eof);
  p->user_callback = callback;

  table_put(&macro_hideset, ident, (void*)ident);
}

//

Token *pp_match(enum TokenKind kind) {
  const char *p = get_lex_p();
  if (p != NULL) {
    for (;;) {
      const char *q = block_comment_start(p);
      if (q == NULL)
        break;

      const char *comment_start = q;
      q += 2;
      for (;;) {
        q = block_comment_end(q);
        if (q != NULL) {
          set_source_string(q, pp_stream->filename, pp_stream->lineno);
          break;
        }

        char *line = NULL;
        size_t capa = 0;
        ssize_t len = getline_cont(&line, &capa, pp_stream->fp, &pp_stream->lineno);
        if (len == -1) {
          lex_error(comment_start, "Block comment not closed");
        }
        q = line;
      }
      p = q;
    }
  }

  return match(kind);
}

Token *pp_consume(enum TokenKind kind, const char *error) {
  Token *tok = pp_match(kind);
  if (tok == NULL)
    pp_parse_error(tok, error);
  return tok;
}

static PpResult expand_ident(const Token *ident) {
  if (!can_expand_ident(ident->ident))
    return 0;

  Vector *tokens = new_vector();
  vec_push(tokens, ident);
  macro_expand(tokens);
  StringBuffer sb;
  sb_init(&sb);
  for (int i = 0; i < tokens->len; ++i) {
    const Token *tok = tokens->data[i];
    sb_append(&sb, tok->begin, tok->end);
  }

  push_lex(ident->ident, NULL);

  char *expanded = sb_to_string(&sb);
  set_source_string(expanded, NULL, -1);

  return pp_prim();
}

static PpResult parse_defined(void) {
  bool lpar = pp_match(TK_LPAR) != NULL;

  const char *start = skip_whitespaces(get_lex_p());
  const char *end = read_ident(start);
  if (end == NULL) {
    lex_error(start, "Ident expected");
  }

  set_source_string(end, pp_stream->filename, pp_stream->lineno);
  if (lpar)
    pp_consume(TK_RPAR, "No close paren");

  return macro_get(alloc_name(start, end, false)) != NULL;
}

static PpResult pp_prim(void) {
  Token *tok;
  if ((tok = pp_match(TK_LPAR)) != NULL) {
    PpResult result = pp_expr();
    pp_consume(TK_RPAR, "No close paren");
    return result;
  }

  if ((tok = pp_match(TK_CHARLIT)) != NULL ||
      (tok = pp_match(TK_INTLIT)) != NULL ||
      (tok = pp_match(TK_LONGLIT)) != NULL ||
      (tok = pp_match(TK_LLONGLIT)) != NULL ||
      (tok = pp_match(TK_UCHARLIT)) != NULL ||
      (tok = pp_match(TK_UINTLIT)) != NULL ||
      (tok = pp_match(TK_ULONGLIT)) != NULL ||
      (tok = pp_match(TK_ULLONGLIT)) != NULL) {
    return tok->fixnum;
  }
  //if ((tok = pp_match(TK_STR)) != NULL)
  //  return new_expr_str(tok, tok->str.buf, tok->str.size);

  Token *ident = pp_consume(TK_IDENT, "Number or Ident or open paren expected");
  if (equal_name(ident->ident, alloc_name("defined", NULL, false)))
    return parse_defined();
  return expand_ident(ident);
}

static PpResult pp_postfix(void) {
  PpResult result = pp_prim();

  //for (;;) {
    //Token *tok;
    //if (pp_match(TK_LPAR))
    //  expr = parse_funcall(expr);
    //else if ((tok = pp_match(TK_LBRACKET)) != NULL)
    //  expr = parse_array_index(tok, expr);
    //else if ((tok = pp_match(TK_INC)) != NULL)
    //  expr = new_expr_unary(EX_POSTINC, NULL, tok, expr);
    //else if ((tok = pp_match(TK_DEC)) != NULL)
    //  expr = new_expr_unary(EX_POSTDEC, NULL, tok, expr);
    //else
      return result;
  //}
}

static PpResult pp_unary(void) {
  Token *tok;
  if ((tok = pp_match(TK_ADD)) != NULL) {
    return pp_cast_expr();
  }

  if ((tok = pp_match(TK_SUB)) != NULL) {
    PpResult result = pp_cast_expr();
    return -result;
  }

  if ((tok = pp_match(TK_NOT)) != NULL) {
    PpResult result = pp_cast_expr();
    return result ? 0 : 1;
  }

  if ((tok = pp_match(TK_TILDA)) != NULL) {
    PpResult result = pp_cast_expr();
    return ~result;
  }

  //if ((tok = pp_match(TK_AND)) != NULL) {
  //  PpExpr *expr = pp_cast_expr();
  //  return new_expr_unary(EX_REF, NULL, tok, expr);
  //}

  //if ((tok = pp_match(TK_MUL)) != NULL) {
  //  PpExpr *expr = pp_cast_expr();
  //  return new_expr_unary(EX_DEREF, NULL, tok, expr);
  //}

  //if ((tok = pp_match(TK_INC)) != NULL) {
  //  PpExpr *expr = pp_unary();
  //  return new_expr_unary(EX_PREINC, NULL, tok, expr);
  //}

  //if ((tok = pp_match(TK_DEC)) != NULL) {
  //  PpExpr *expr = pp_unary();
  //  return new_expr_unary(EX_PREDEC, NULL, tok, expr);
  //}

  return pp_postfix();
}

static PpResult pp_cast_expr(void) {
  return pp_unary();
}

static PpResult pp_mul(void) {
  PpResult result = pp_cast_expr();
  for (;;) {
    Token *tok;
    if (!(((tok = pp_match(TK_MUL)) != NULL) ||
          ((tok = pp_match(TK_DIV)) != NULL) ||
          ((tok = pp_match(TK_MOD)) != NULL)))
      return result;

    PpResult rhs = pp_cast_expr();
    switch (tok->kind) {
    case TK_MUL:  result *= rhs; break;
    case TK_DIV:  result /= rhs; break;
    case TK_MOD:  result %= rhs; break;
    default:  assert(false); break;
    }
  }
}

static PpResult pp_add(void) {
  PpResult result = pp_mul();
  for (;;) {
    Token *tok;
    if (!(((tok = pp_match(TK_ADD)) != NULL) ||
          ((tok = pp_match(TK_SUB)) != NULL)))
      return result;

    PpResult rhs = pp_mul();
    if (tok->kind == TK_ADD)
      result += rhs;
    else
      result -= rhs;
  }
}

static PpResult pp_shift(void) {
  PpResult result = pp_add();
  for (;;) {
    Token *tok;
    if (!(((tok = pp_match(TK_LSHIFT)) != NULL) ||
          ((tok = pp_match(TK_RSHIFT)) != NULL)))
      return result;

    PpResult lhs = result, rhs = pp_add();
    if (tok->kind == TK_LSHIFT)
      result = lhs << rhs;
    else
      result = lhs >> rhs;
  }
}

static PpResult pp_cmp(void) {
  PpResult result = pp_shift();
  for (;;) {
    Token *tok;
    if (!(((tok = pp_match(TK_LT)) != NULL) ||
          ((tok = pp_match(TK_GT)) != NULL) ||
          ((tok = pp_match(TK_LE)) != NULL) ||
          ((tok = pp_match(TK_GE)) != NULL)))
      return result;

    PpResult lhs = result, rhs = pp_shift();
    switch (tok->kind) {
    case TK_LT:  result = lhs <  rhs ? 1 : 0; break;
    case TK_LE:  result = lhs <= rhs ? 1 : 0; break;
    case TK_GE:  result = lhs >= rhs ? 1 : 0; break;
    case TK_GT:  result = lhs >  rhs ? 1 : 0; break;
    default:  assert(false); break;
    }
  }
}

static PpResult pp_eq(void) {
  PpResult result = pp_cmp();
  for (;;) {
    Token *tok;
    if (!(((tok = pp_match(TK_EQ)) != NULL) ||
          ((tok = pp_match(TK_NE)) != NULL)))
      return result;

    PpResult lhs = result, rhs = pp_cmp();
    result = lhs == rhs ? 1 : 0;
    if (tok->kind != TK_EQ)
      result = 1 - result;
  }
}

static PpResult pp_and(void) {
  PpResult result = pp_eq();
  for (;;) {
    Token *tok;
    if ((tok = pp_match(TK_AND)) != NULL) {
      PpResult lhs = result, rhs = pp_eq();
      result = lhs & rhs;
    } else
      return result;
  }
}

static PpResult pp_xor(void) {
  PpResult result = pp_and();
  for (;;) {
    Token *tok;
    if ((tok = pp_match(TK_HAT)) != NULL) {
      PpResult lhs = result, rhs = pp_and();
      result = lhs ^ rhs;
    } else
      return result;
  }
}

static PpResult pp_or(void) {
  PpResult result = pp_xor();
  for (;;) {
    Token *tok;
    if ((tok = pp_match(TK_OR)) != NULL) {
      PpResult lhs = result, rhs = pp_xor();
      result = lhs | rhs;
    } else
      return result;
  }
}

static PpResult pp_logand(void) {
  PpResult result = pp_or();
  for (;;) {
    Token *tok;
    if ((tok = pp_match(TK_LOGAND)) != NULL) {
      PpResult rhs = pp_logand();
      result = result && rhs;
    } else
      return result;
  }
}

static PpResult pp_logior(void) {
  PpResult result = pp_logand();
  for (;;) {
    Token *tok;
    if ((tok = pp_match(TK_LOGIOR)) != NULL) {
      PpResult rhs = pp_logand();
      result = result || rhs;
    } else
      return result;
  }
}

static PpResult pp_conditional(void) {
  PpResult result = pp_logior();
  for (;;) {
    const Token *tok;
    if ((tok = pp_match(TK_QUESTION)) == NULL)
      return result;
    PpResult tval = pp_expr();
    pp_consume(TK_COLON, "`:' expected");
    PpResult fval = pp_conditional();

    result = result ? tval : fval;
  }
}

static PpResult pp_assign(void) {
  return pp_conditional();
}

PpResult pp_expr(void) {
  PpResult result = pp_assign();
  const Token *tok;
  while ((tok = pp_match(TK_COMMA)) != NULL) {
    PpResult next_result = pp_assign();
    result = next_result;
  }
  return result;
}

static Token *match2(enum TokenKind kind) {
  while (pp_match(TK_EOF)) {
    char *line = NULL;
    size_t capa = 0;
    ssize_t len = getline(&line, &capa, pp_stream->fp);
    if (len == -1)
      return NULL;
    ++pp_stream->lineno;
    set_source_string(line, pp_stream->filename, pp_stream->lineno);
  }
  return pp_match(kind);
}

Token *match3(enum TokenKind kind, Vector *tokens, int *pindex) {
  int index = *pindex;
  Token *tok;
  if (index < tokens->len) {
    tok = tokens->data[index];
    if ((int)kind == -1 || tok->kind == kind)
      ++index;
    else
      tok = NULL;
  } else {
    tok = match2(kind);
    if (tok != NULL && tok->kind != TK_EOF) {
      vec_push(tokens, tok);
      ++index;
    }
  }
  *pindex = index;
  return tok;
}

Vector *pp_funargs(Vector *tokens, int *pindex, int vaarg) {
  Vector *args = NULL;
  if (match3(TK_LPAR, tokens, pindex)) {
    args = new_vector();
    Vector *arg = NULL;
    int paren = 0;
    bool need_space = false;
    const Token *tok_space = NULL;
    for (;;) {
      const char *start = get_lex_p();
      bool fetched = *pindex >= tokens->len;
      Token *tok = match3(-1, tokens, pindex);
      if (tok == NULL /*|| tok->kind == TK_EOF*/) {
        pp_parse_error(NULL, "`)' expected");
        break;
      }

      if (tok->kind == TK_LPAR) {
        ++paren;
      } else if (tok->kind == TK_RPAR) {
        if (paren <= 0)
          break;
        --paren;
      }

      if (arg == NULL)
        arg = new_vector();

      if (tok->kind == TK_COMMA && paren <= 0 && args->len < vaarg) {
        vec_push(args, arg);
        arg = new_vector();
        continue;
      }
      if (need_space || (fetched && tok->begin != start)) {
        if (arg->len > 0) {
          if (tok_space == NULL)
            tok_space = alloc_token(PPTK_SPACE, " ", NULL);
          vec_push(arg, tok_space);
        }
        need_space = false;
      }
      if (tok->kind != PPTK_SPACE || arg->len > 0)
        vec_push(arg, tok);
    }
    if (arg != NULL)
      vec_push(args, arg);

    if (args->len == vaarg)
      vec_push(args, new_vector());
  }
  return args;
}
