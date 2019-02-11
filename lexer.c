#include <assert.h>
#include <ctype.h>
#include <stdlib.h>  // malloc
#include <string.h>
#include <sys/types.h>  // ssize_t

#include "xcc.h"

char *strndup_(const char *str, size_t size) {
  char *dup = malloc(size + 1);
  strncpy(dup, str, size);
  dup[size] = '\0';
  return dup;
}

ssize_t getline_(char **lineptr, size_t *n, FILE *stream) {
  const int ADD = 16;
  ssize_t capa = *n;
  ssize_t size = 0;
  char *top = *lineptr;
  for (;;) {
    int c = fgetc(stream);
    if (c == EOF) {
      if (size == 0)
        return EOF;
      top[size] = '\0';
      *lineptr = top;
      *n = capa;
      return size;
    }

    if (size + 2 > capa) {
      ssize_t newcapa = capa + ADD;
      top = realloc(top, newcapa);
      if (top == NULL)
        return EOF;
      capa = newcapa;
    }
    top[size++] = c;
  }
}

typedef struct {
  FILE *fp;
  char* line;
  size_t line_len;
  const char *p;
  Token *tok;
} Lexer;

static Lexer lexer;

static Token *alloc_token(enum TokenType type, const char *input) {
  Token *token = malloc(sizeof(*token));
  token->type = type;
  token->input = input;
  return token;
}

enum TokenType reserved_word(const char *word) {
  struct {
    const char *str;
    enum TokenType type;
  } table[] = {
    { "if", TK_IF },
    { "else", TK_ELSE },
    { "do", TK_DO },
    { "while", TK_WHILE },
    { "for", TK_FOR },
    { "return", TK_RETURN },
    { "void", TK_KWVOID },
    { "int", TK_KWINT },
    { "char", TK_KWCHAR },
    { "struct", TK_STRUCT },
  };
  for (int i = 0; i < sizeof(table) / sizeof(*table); ++i) {
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
  lexer.tok = NULL;
}

static Token *get_token(void) {
  Token *tok = NULL;
  const char *p = lexer.p;
  if (p == NULL)
    return alloc_token(TK_EOF, NULL);

  for (;;) {
    for (;; ++p) {
      if (*p == '\0') {
        lexer.line = NULL;
        lexer.line_len = 0;
        if (getline_(&lexer.line, &lexer.line_len, lexer.fp) == EOF) {
          lexer.p = NULL;
          return alloc_token(TK_EOF, NULL);
        }
        p = lexer.line;
      }
      if (!isspace(*p))
        break;
    }

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

    if (*p == '+' && p[1] == '+') {
      tok = alloc_token(TK_INC, p);
      p += 2;
      break;
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
    }

    if (strchr("+-*/%&(){}[]<>=;,.", *p) != NULL) {
      tok = alloc_token((enum TokenType)*p, p);
      ++p;
      break;
    }

    if (isdigit(*p)) {
      long val = strtol(p, (char**)&p, 10);
      tok = alloc_token(TK_NUM, p);
      tok->val = val;
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
      if (word != -1) {
        free(dup);
        tok = alloc_token(word, p);
      } else {
        tok= alloc_token(TK_IDENT, p);
        tok->ident = dup;
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

      tok = alloc_token(TK_CHAR, start);
      tok->val = c;
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
      tok->str = str;
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
  if (lexer.tok == NULL)
    lexer.tok = get_token();
  Token *tok = lexer.tok;
  if (tok->type != type)
    return NULL;
  if (tok->type != TK_EOF)
    lexer.tok = NULL;
  return tok;
}

const char *current_line(void) {
  return lexer.p;
}
