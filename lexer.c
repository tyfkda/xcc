#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>  // malloc
#include <string.h>
#include <sys/types.h>  // ssize_t

#include "xcc.h"
#include "util.h"

#define MAX_LOOKAHEAD  (2)

typedef struct {
  FILE *fp;
  const char *filename;
  Line* line;
  const char *p;
  Token *fetched[MAX_LOOKAHEAD];
  int idx;
  int lineno;
} Lexer;

static Lexer lexer;

void show_error_line(const char *line, const char *p) {
  fprintf(stderr, "%s\n", line);
  size_t pos = p - line;
  if (pos <= strlen(line)) {
    for (size_t i = 0; i < pos; ++i)
      fputc(line[i] == '\t' ? '\t' : ' ', stderr);
    fprintf(stderr, "^\n");
  }
}

void lex_error(const char *p, const char* fmt, ...) {
  fprintf(stderr, "%s(%d): ", lexer.filename, lexer.lineno);

  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");

  show_error_line(lexer.line->buf, p);

  exit(1);
}

void parse_error(const Token *token, const char* fmt, ...) {
  if (token == NULL)
    token = fetch_token();
  if (token != NULL) {
    fprintf(stderr, "%s(%d): ", token->line->filename, token->line->lineno);
  }

  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");

  show_error_line(token->line->buf, token->begin);

  exit(1);
}

static Token *alloc_token(enum TokenType type, const char *begin, const char *end) {
  Token *token = malloc(sizeof(*token));
  token->type = type;
  token->line = lexer.line;
  token->begin = begin;
  token->end = end;
  return token;
}

Token *alloc_ident(const char *ident, const char *begin, const char *end) {
  Token *tok = alloc_token(TK_IDENT, begin, end);
  tok->u.ident = ident;
  return tok;
}

static enum TokenType reserved_word(const char *word) {
  struct {
    const char *str;
    enum TokenType type;
  } table[] = {
    { "if", TK_IF },
    { "else", TK_ELSE },
    { "switch", TK_SWITCH },
    { "case", TK_CASE },
    { "default", TK_DEFAULT },
    { "do", TK_DO },
    { "while", TK_WHILE },
    { "for", TK_FOR },
    { "break", TK_BREAK },
    { "continue", TK_CONTINUE },
    { "return", TK_RETURN },
    { "void", TK_KWVOID },
    { "char", TK_KWCHAR },
    { "short", TK_KWSHORT },
    { "int", TK_KWINT },
    { "long", TK_KWLONG },
    { "const", TK_KWCONST },
    { "unsigned", TK_UNSIGNED },
    { "static", TK_STATIC },
    { "extern", TK_EXTERN },
    { "struct", TK_STRUCT },
    { "union", TK_UNION },
    { "enum", TK_ENUM },
    { "sizeof", TK_SIZEOF },
    { "typedef", TK_TYPEDEF },
  };
  for (int i = 0; i < (int)(sizeof(table) / sizeof(*table)); ++i) {
    if (strcmp(table[i].str, word) == 0)
      return table[i].type;
  }
  return -1;
}

static char backslash(char c) {
  switch (c) {
  case '0':  return '\0';
  case 'n':  return '\n';
  case 't':  return '\t';
  case 'r':  return '\r';
  default:   return c;
  }
}

void init_lexer(FILE *fp, const char *filename) {
  lexer.fp = fp;
  lexer.filename = filename;
  lexer.line = NULL;
  lexer.p = "";
  lexer.idx = -1;
  lexer.lineno = 0;
}

void init_lexer_string(const char *line, const char *filename, int lineno) {
  Line *p = malloc(sizeof(*line));
  p->filename = lexer.filename;
  p->buf = line;
  p->lineno = lineno;

  lexer.fp = NULL;
  lexer.filename = filename;
  lexer.line = p;
  lexer.p = line;
  lexer.idx = -1;
  lexer.lineno = lineno;
}

const char *get_lex_p(void) {
  return lexer.p;
}

static void read_next_line(void) {
  if (lexer.fp == NULL) {
    lexer.p = NULL;
    lexer.line = NULL;
    return;
  }

  char *line = NULL;
  size_t capa = 0;
  ssize_t len = getline_(&line, &capa, lexer.fp);
  if (len == EOF) {
    lexer.p = NULL;
    lexer.line = NULL;
  } else {
    Line *p = malloc(sizeof(*line));
    p->filename = lexer.filename;
    p->buf = line;
    p->lineno = ++lexer.lineno;
    lexer.line = p;
    lexer.p = lexer.line->buf;
  }
}

static const char *skip_block_comment(const char *p) {
  for (;;) {
    char c = *p++;
    if (c == '\0') {
      read_next_line();
      p = lexer.p;
      if (p == NULL)
        return NULL;
      continue;
    } else if (c == '*' && *p == '/')
      return p + 1;
  }
}

static const char *skip_line_comment(void) {
  read_next_line();
  return lexer.p;
}

static const char *skip_whitespace_or_comment(const char *p) {
  for (;;) {
    char c = *p++;
    if (c == '\0') {
      read_next_line();
      p = lexer.p;
      if (p == NULL)
        return NULL;
      continue;
    } else if (isspace(c)) {
      continue;
    } else if (c == '/') {
      if (*p == '*') {
        p = skip_block_comment(p + 1);
        if (p == NULL)
          return NULL;
        continue;
      } else if (*p == '/') {
        p = skip_line_comment();
        if (p == NULL)
          return NULL;
        continue;
      }
    }
    break;
  }
  return p - 1;
}

static Token *read_num(const char **pp) {
  const char *start = *pp, *p = start;
  int base = 10;
  if (*p == '0') {
    base = 8;
    ++p;
    if (*p == 'x') {
      base = 16;
      ++p;
    }
  }
  long val = strtol(p, (char**)pp, base);
  if (*pp == p && base == 16)
    lex_error(p, "Illegal literal");
  Token *tok;
  enum TokenType tt = TK_INTLIT;
  if (**pp == 'L') {
    tt = TK_LONGLIT;
    ++(*pp);
  }
  tok = alloc_token(tt, start, *pp);
  tok->u.value = val;
  return tok;
}

char *read_ident(const char **pp) {
  const char *p = *pp;
  if (isalpha(*p) || *p == '_') {
    const char *q;
    for (q = p + 1; ; ++q) {
      if (!(isalnum(*q) || *q == '_'))
        break;
    }
    *pp = q;
    return strndup_(p, q - p);
  }
  return NULL;
}

static Token *get_token(void) {
  Token *tok = NULL;
  const char *p = lexer.p;
  if (p == NULL)
    return alloc_token(TK_EOF, NULL, NULL);

  for (;;) {
    p = skip_whitespace_or_comment(p);
    if (p == NULL)
      return alloc_token(TK_EOF, NULL, NULL);

    if (*p == '=' && p[1] == '=') {
      tok = alloc_token(TK_EQ, p, p + 2);
      p += 2;
      break;
    }

    if (*p == '!' && p[1] == '=') {
      tok = alloc_token(TK_NE, p, p + 2);
      p += 2;
      break;
    }

    if (*p == '<') {
      if (p[1] == '=') {
        tok = alloc_token(TK_LE, p, p + 2);
        p += 2;
        break;
      }
      if (p[1] == '<') {
        tok = alloc_token(TK_LSHIFT, p, p + 2);
        p += 2;
        break;
      }
    }

    if (*p == '>') {
      if (p[1] == '=') {
        tok = alloc_token(TK_GE, p, p + 2);
        p += 2;
        break;
      }
      if (p[1] == '>') {
        tok = alloc_token(TK_RSHIFT, p, p + 2);
        p += 2;
        break;
      }
    }

    if (*p == '+') {
      if (p[1] == '+') {
        tok = alloc_token(TK_INC, p, p + 2);
        p += 2;
        break;
      }
      if (p[1] == '=') {
        tok = alloc_token(TK_ADD_ASSIGN, p, p + 2);
        p += 2;
        break;
      }
    }

    if (*p == '-') {
      if (p[1] == '-') {
        tok = alloc_token(TK_DEC, p, p + 2);
        p += 2;
        break;
      }
      if (p[1] == '>') {
        tok = alloc_token(TK_ARROW, p, p + 2);
        p += 2;
        break;
      }
      if (p[1] == '=') {
        tok = alloc_token(TK_SUB_ASSIGN, p, p + 2);
        p += 2;
        break;
      }
    }

    if (*p == '*' && p[1] == '=') {
      tok = alloc_token(TK_MUL_ASSIGN, p, p + 2);
      p += 2;
      break;
    }

    if (*p == '/' && p[1] == '=') {
      tok = alloc_token(TK_DIV_ASSIGN, p, p + 2);
      p += 2;
      break;
    }

    if (*p == '%' && p[1] == '=') {
      tok = alloc_token(TK_MOD_ASSIGN, p, p + 2);
      p += 2;
      break;
    }

    if (*p == '&' && p[1] == '&') {
      tok = alloc_token(TK_LOGAND, p, p + 2);
      p += 2;
      break;
    }

    if (*p == '|') {
      if (p[1] == '|') {
        tok = alloc_token(TK_LOGIOR, p, p + 2);
        p += 2;
        break;
      }
    }

    if (*p == '.' && p[1] == '.' && p[2] == '.') {
      tok = alloc_token(TK_DOTDOTDOT, p, p + 3);
      p += 3;
      break;
    }

    if (strchr("+-*/%&!(){}[]<>=^|:;,.?", *p) != NULL) {
      tok = alloc_token((enum TokenType)*p, p, p + 1);
      ++p;
      break;
    }

    if (isdigit(*p)) {
      tok = read_num(&p);
      break;
    }

    const char *begin = p;
    char *ident = read_ident(&p);
    if (ident != NULL) {
      enum TokenType word = reserved_word(ident);
      if ((int)word != -1) {
        free(ident);
        tok = alloc_token(word, begin, p);
      } else {
        tok = alloc_ident(ident, begin, p);
      }
      break;
    }

    if (*p == '\'') {
      const char *begin = p++;
      char c = *p;
      if (c == '\\') {
        c = *(++p);
        if (c == '\0')
          lex_error(p, "Character not closed");
        c = backslash(c);
      }
      if (*(++p) != '\'')
        lex_error(p, "Character not closed");

      ++p;
      tok = alloc_token(TK_CHARLIT, begin, p);
      tok->u.value = c;
      break;
    }

    if (*p == '"') {
      const char *begin = p++;
      size_t capa = 8, size = 0;
      char *str = malloc(capa);
      for (char c; (c = *p) != '"'; ++p) {
        if (c == '\0')
          lex_error(p, "String not closed");
        if (size + 1 >= capa) {
          capa <<= 1;
          str = realloc(str, capa);
        }

        if (c == '\\') {
          c = *(++p);
          if (c == '\0')
            lex_error(p, "String not closed");
          c = backslash(c);
        }
        str[size++] = c;
      }
      str[size] = '\0';
      ++p;
      tok = alloc_token(TK_STR, begin, p);
      tok->u.str.buf = str;
      tok->u.str.len = size + 1;
      break;
    }

    lex_error(p, "Unexpected character `%c'", *p);
    return NULL;
  }

  assert(tok != NULL);
  lexer.p = p;
  return tok;
}

Token *fetch_token(void) {
  if (lexer.idx < 0) {
    lexer.idx = 0;
    lexer.fetched[0] = get_token();
  }
  return lexer.fetched[lexer.idx];
}

Token *consume(enum TokenType type) {
  Token *tok = fetch_token();
  if (tok->type != type && (int)type != -1)
    return NULL;
  if (tok->type != TK_EOF)
    --lexer.idx;
  return tok;
}

void unget_token(Token *token) {
  ++lexer.idx;
  assert(lexer.idx < MAX_LOOKAHEAD);
  lexer.fetched[lexer.idx] = token;
}
