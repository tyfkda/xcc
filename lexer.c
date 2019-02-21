#include <assert.h>
#include <ctype.h>
#include <stdlib.h>  // malloc
#include <string.h>
#include <sys/types.h>  // ssize_t

#include "xcc.h"
#include "util.h"

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

static enum TokenType reserved_word(const char *word) {
  struct {
    const char *str;
    enum TokenType type;
  } table[] = {
    { "if", TK_IF },
    { "else", TK_ELSE },
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
  lexer.tok = NULL;
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
  if (**pp == 'L') {
    tok = alloc_token(TK_LONGLIT, start);
    tok->longval = val;
    ++(*pp);
  } else {
    tok = alloc_token(TK_INTLIT, start);
    tok->intval = val;
  }
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

    if (strchr("+-*/%&!(){}[]<>=;,.", *p) != NULL) {
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

      tok = alloc_token(TK_CHARLIT, start);
      tok->charval = c;
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
  return lexer.line;
}
