#include "../config.h"
#include "pp_parser.h"

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>  // exit
#include <string.h>

#include "lexer.h"
#include "macro.h"
#include "preprocessor.h"
#include "table.h"
#include "util.h"

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
    lex_stack.buf = realloc_or_die(lex_stack.buf, sizeof(*lex_stack.buf) * lex_stack.capacity);
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

static PpResult parse_defined(void) {
  bool lpar = pp_match(TK_LPAR) != NULL;

  const char *start = skip_whitespaces(get_lex_p());
  const char *end = read_ident(start);
  if (end == NULL) {
    lex_error(start, "ident expected");
  }

  set_source_string(end, pp_stream->filename, pp_stream->lineno);
  if (lpar)
    pp_consume(TK_RPAR, "No close paren");

  return macro_get(alloc_name(start, end, false)) != NULL;
}

static Token *match2(enum TokenKind kind) {
  while (pp_match(TK_EOF)) {
    const char *line = get_processed_next_line();
    if (line == NULL)
      return NULL;
    set_source_string(line, pp_stream->filename, pp_stream->lineno);
  }
  return pp_match(kind);
}

static Token *match3(enum TokenKind kind, Vector *tokens, int *pindex) {
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
  while (match3(PPTK_SPACE, tokens, pindex))
    ;
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
            tok_space = alloc_token(PPTK_SPACE, NULL, " ", NULL);
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

// Pratt parser.

typedef enum {
  PREC_NONE,
  PREC_COMMA,    // ,
  PREC_ASSIGN,   // =
  PREC_TERNARY,  // ?:
  PREC_LOGIOR,   // ||
  PREC_LOGAND,   // &&
  PREC_BITOR,    // |
  PREC_BITXOR,   // ^
  PREC_BITAND,   // &
  PREC_EQ,       // == !=
  PREC_CMP,      // < > <= >=
  PREC_SHIFT,    // << >>
  PREC_TERM,     // + -
  PREC_FACTOR,   // * /
  PREC_POSTFIX,  // ++ -- . -> [] ()
} Precedence;

typedef PpResult (*ParsePrefixFn)(Token *token);
typedef PpResult (*ParseInfixFn)(PpResult lhs, Token *token);

typedef struct {
  ParsePrefixFn prefix;
  ParseInfixFn infix;
  Precedence precedence;
} ParseRule;

static const ParseRule *get_rule(enum TokenKind kind);

static PpResult parse_precedence(Precedence precedence) {
  Token *previous = match(-1);
  ParsePrefixFn prefixRule = get_rule(previous->kind)->prefix;
  if (prefixRule == NULL) {
    pp_parse_error(previous, "Expect expression.");
  }

  PpResult expr = prefixRule(previous);

  for (;;) {
    Token *current = fetch_token();
    const ParseRule *rule = get_rule(current->kind);
    assert(rule != NULL);
    if (precedence > rule->precedence)
      break;
    ParseInfixFn infixRule = rule->infix;
    assert(infixRule != NULL);
    expr = infixRule(expr, match(-1));
    previous = current;
  }
  return expr;
}

static PpResult literal(Token *tok) {
  switch (tok->kind) {
  case TK_INTLIT:
    return tok->fixnum.value;
  // case TK_STR:     return string_expr(tok, tok->str.buf, tok->str.len, tok->str.kind);
#ifndef __NO_FLONUM
  case TK_FLOATLIT: case TK_DOUBLELIT: case TK_LDOUBLELIT:
    return tok->flonum;
#endif
  default: assert(false); return 0;  // Unreachable.
  }
}

static PpResult variable(Token *ident) {
  if (equal_name(ident->ident, alloc_name("defined", NULL, false)))
    return parse_defined();
  return 0;  // Undefined identifier is 0.
}

static PpResult unary(Token *tok) {
  PpResult expr = parse_precedence(PREC_POSTFIX);

  enum TokenKind kind = tok->kind;
  switch (kind) {
  case TK_ADD: return expr;
  case TK_SUB: return -expr;
  case TK_NOT: return expr ? 0 : 1;
  case TK_TILDA: return ~expr;
  default: assert(false); return 0;  // Unreachable.
  }
}

static PpResult binary(PpResult lhs, Token *tok) {
  const ParseRule* rule = get_rule(tok->kind);
  assert(rule != NULL);
  PpResult rhs = parse_precedence(rule->precedence + 1);

  enum TokenKind kind = tok->kind;
  switch (kind) {
  case TK_ADD:  return lhs + rhs;
  case TK_SUB:  return lhs - rhs;
  case TK_MUL:  return lhs * rhs;
  case TK_DIV:
    if (rhs != 0) {
      return lhs / rhs;
    } else {
      // pp_parse_error(tok, "Division by zero");
      return lhs;
    }
  case TK_MOD:
    if (rhs != 0) {
      return lhs % rhs;
    } else {
      // pp_parse_error(tok, "Modulo by zero");
      return lhs;
    }
  case TK_LSHIFT:  return lhs << rhs;
  case TK_RSHIFT:  return lhs >> rhs;
  case TK_AND:  return lhs & rhs;
  case TK_OR:   return lhs | rhs;
  case TK_HAT:  return lhs ^ rhs;
  case TK_EQ:  return lhs == rhs;
  case TK_NE:  return lhs != rhs;
  case TK_LT:  return lhs < rhs;
  case TK_LE:  return lhs <= rhs;
  case TK_GE:  return lhs >= rhs;
  case TK_GT:  return lhs > rhs;
  case TK_LOGAND:  return lhs && rhs;
  case TK_LOGIOR:  return lhs || rhs;
  case TK_COMMA:  return rhs;

  default: assert(false); return 0;  // Unreachable.
  }
}

static PpResult ternary(PpResult expr, Token *tok) {
  const ParseRule* rule = get_rule(tok->kind);
  assert(rule != NULL);
  PpResult tval = pp_expr();

  pp_consume(TK_COLON, "`:' expected");
  PpResult fval = parse_precedence(rule->precedence);

  return expr ? tval : fval;
}

static PpResult grouping(Token *tok) {
  UNUSED(tok);
  PpResult expr = pp_expr();
  pp_consume(TK_RPAR, "Expect ')' after expression.");
  return expr;
}

static const ParseRule *get_rule(enum TokenKind kind) {
  static const ParseRule kRules[] = {
    [TK_LPAR]          = {grouping},

    [TK_MUL]           = {unary,    binary,    PREC_FACTOR},
    [TK_DIV]           = {NULL,     binary,    PREC_FACTOR},
    [TK_MOD]           = {NULL,     binary,    PREC_FACTOR},

    [TK_ADD]           = {unary,    binary,    PREC_TERM},
    [TK_SUB]           = {unary,    binary,    PREC_TERM},

    [TK_LSHIFT]        = {NULL,     binary,    PREC_SHIFT},
    [TK_RSHIFT]        = {NULL,     binary,    PREC_SHIFT},

    [TK_LT]            = {NULL,     binary,    PREC_CMP},
    [TK_LE]            = {NULL,     binary,    PREC_CMP},
    [TK_GE]            = {NULL,     binary,    PREC_CMP},
    [TK_GT]            = {NULL,     binary,    PREC_CMP},

    [TK_EQ]            = {NULL,     binary,    PREC_EQ},
    [TK_NE]            = {NULL,     binary,    PREC_EQ},

    [TK_AND]           = {unary,    binary,    PREC_BITAND},
    [TK_HAT]           = {NULL,     binary,    PREC_BITXOR},
    [TK_OR]            = {NULL,     binary,    PREC_BITOR},

    [TK_LOGAND]        = {NULL,     binary,    PREC_LOGAND},
    [TK_LOGIOR]        = {NULL,     binary,    PREC_LOGIOR},

    [TK_QUESTION]      = {NULL,     ternary,   PREC_TERNARY},

    [TK_COMMA]         = {NULL,     binary,    PREC_COMMA},

    [TK_NOT]           = {unary},
    [TK_TILDA]         = {unary},

    [TK_INTLIT]        = {literal},
    [TK_STR]           = {literal},
    [TK_FLOATLIT]      = {literal},
    [TK_DOUBLELIT]     = {literal},
    [TK_LDOUBLELIT]    = {literal},

    [TK_IDENT]         = {variable},

    [TK_EOF]           = {NULL},
  };

  if (kind >= ARRAY_SIZE(kRules))
    kind = TK_EOF;
  return &kRules[kind];
}

PpResult pp_expr(void) {
  return parse_precedence(PREC_COMMA);
}
