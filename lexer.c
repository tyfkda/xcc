#include <assert.h>
#include <ctype.h>
#include <stdlib.h>  // malloc
#include <string.h>
#include <sys/types.h>  // ssize_t

#include "xcc.h"
#include "util.h"

#define MAX_LOOKAHEAD  (2)

typedef struct {
  FILE *fp;
  char* line;
  size_t line_len;
  const char *p;
  Token *fetched[MAX_LOOKAHEAD];
  int idx;
} Lexer;

static Lexer lexer;

static Token *alloc_token(enum TokenType type, const char *input) {
  Token *token = malloc(sizeof(*token));
  token->type = type;
  token->input = input;
  return token;
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
    { "int", TK_KWINT },
    { "long", TK_KWLONG },
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

void init_lexer(FILE *fp) {
  lexer.fp = fp;
  lexer.line = NULL;
  lexer.line_len = 0;
  lexer.p = "";
  lexer.idx = -1;
}

static void read_next_line(void) {
  lexer.line = NULL;
  lexer.line_len = 0;
  if (getline_(&lexer.line, &lexer.line_len, lexer.fp) == EOF) {
    lexer.p = NULL;
  }
  lexer.p = lexer.line;
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
    error("Illegal literal: %s", current_line());
  Token *tok;
  enum TokenType tt = TK_INTLIT;
  if (**pp == 'L') {
    tt = TK_LONGLIT;
    ++(*pp);
  }
  tok = alloc_token(tt, start);
  tok->u.value = val;
  return tok;
}

static Token *get_token(void) {
  Token *tok = NULL;
  const char *p = lexer.p;
  if (p == NULL)
    return alloc_token(TK_EOF, NULL);

  for (;;) {
    p = skip_whitespace_or_comment(p);
    if (p == NULL)
      return alloc_token(TK_EOF, NULL);

    if (*p == '=' && p[1] == '=') {
      tok = alloc_token(TK_EQ, p);
      p += 2;
      break;
    }

    if (*p == '!' && p[1] == '=') {
      tok = alloc_token(TK_NE, p);
      p += 2;
      break;
    }

    if (*p == '<' && p[1] == '=') {
      tok = alloc_token(TK_LE, p);
      p += 2;
      break;
    }

    if (*p == '>' && p[1] == '=') {
      tok = alloc_token(TK_GE, p);
      p += 2;
      break;
    }

    if (*p == '+') {
      if (p[1] == '+') {
        tok = alloc_token(TK_INC, p);
        p += 2;
        break;
      }
      if (p[1] == '=') {
        tok = alloc_token(TK_ADD_ASSIGN, p);
        p += 2;
        break;
      }
    }

    if (*p == '-') {
      if (p[1] == '-') {
        tok = alloc_token(TK_DEC, p);
        p += 2;
        break;
      }
      if (p[1] == '>') {
        tok = alloc_token(TK_ARROW, p);
        p += 2;
        break;
      }
      if (p[1] == '=') {
        tok = alloc_token(TK_SUB_ASSIGN, p);
        p += 2;
        break;
      }
    }

    if (*p == '*' && p[1] == '=') {
      tok = alloc_token(TK_MUL_ASSIGN, p);
      p += 2;
      break;
    }

    if (*p == '/' && p[1] == '=') {
      tok = alloc_token(TK_DIV_ASSIGN, p);
      p += 2;
      break;
    }

    if (*p == '%' && p[1] == '=') {
      tok = alloc_token(TK_MOD_ASSIGN, p);
      p += 2;
      break;
    }

    if (*p == '&' && p[1] == '&') {
      tok = alloc_token(TK_LOGAND, p);
      p += 2;
      break;
    }

    if (*p == '|' && p[1] == '|') {
      tok = alloc_token(TK_LOGIOR, p);
      p += 2;
      break;
    }

    if (strchr("+-*/%&!(){}[]<>=:;,.", *p) != NULL) {
      tok = alloc_token((enum TokenType)*p, p);
      ++p;
      break;
    }

    if (isdigit(*p)) {
      tok = read_num(&p);
      break;
    }

    if (isalpha(*p) || *p == '_') {
      const char *q;
      for (q = p + 1; ; ++q) {
        if (!(isalnum(*q) || *q == '_'))
          break;
      }

      char *dup = strndup_(p, q - p);
      enum TokenType word = reserved_word(dup);
      if ((int)word != -1) {
        free(dup);
        tok = alloc_token(word, p);
      } else {
        tok= alloc_token(TK_IDENT, p);
        tok->u.ident = dup;
      }
      p = q;
      break;
    }

    if (*p == '\'') {
      const char *start = p++;
      char c = *p;
      if (c == '\\') {
        c = *(++p);
        if (c == '\0')
          error("Character not closed");
        c = backslash(c);
      }
      if (*(++p) != '\'')
        error("Character not closed");

      tok = alloc_token(TK_CHARLIT, start);
      tok->u.value = c;
      ++p;
      break;
    }

    if (*p == '"') {
      const char *start = p++;
      size_t capa = 8, size = 0;
      char *str = malloc(capa);
      for (char c; (c = *p) != '"'; ++p) {
        if (c == '\0')
          error("String not closed");
        if (size + 1 >= capa) {
          capa <<= 1;
          str = realloc(str, capa);
        }

        if (c == '\\') {
          c = *(++p);
          if (c == '\0')
            error("String not closed");
          c = backslash(c);
        }
        str[size++] = c;
      }
      str[size] = '\0';
      tok = alloc_token(TK_STR, start);
      tok->u.str.buf = str;
      tok->u.str.len = size + 1;
      ++p;
      break;
    }

    error("Unexpected character `%c' at %s\n", *p, p);
    return NULL;
  }

  assert(tok != NULL);
  lexer.p = p;
  return tok;
}

Token *consume(enum TokenType type) {
  if (lexer.idx < 0) {
    lexer.idx = 0;
    lexer.fetched[0] = get_token();
  }
  Token *tok = lexer.fetched[lexer.idx];
  if (tok->type != type)
    return NULL;
  if (tok->type != TK_EOF)
    --lexer.idx;
  return tok;
}

void unget_token(Token *token) {
  assert(++lexer.idx < MAX_LOOKAHEAD);
  lexer.fetched[lexer.idx] = token;
}

const char *current_line(void) {
  //return lexer.line;
  return lexer.p;
}
