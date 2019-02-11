#include <ctype.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "xcc.h"

char *strndup_(const char *str, size_t size) {
  char *dup = malloc(size + 1);
  strncpy(dup, str, size);
  dup[size] = '\0';
  return dup;
}

static Vector *token_vector;

Token *alloc_token(enum TokenType type, const char *input) {
  Token *token = malloc(sizeof(*token));
  token->type = type;
  token->input = input;
  vec_push(token_vector, token);
  return token;
}

Token *get_token(int pos) {
  return (Token *)token_vector->data[pos];
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

char backslash(char c) {
  switch (c) {
  case '0':  return '\0';
  case 'n':  return '\n';
  case 't':  return '\t';
  case 'r':  return '\r';
  default:   return c;
  }
}

void tokenize(const char *p) {
  token_vector = new_vector();

  while (*p != '\0') {
    if (isspace(*p)) {
      ++p;
      continue;
    }

    if (*p == '=' && p[1] == '=') {
      alloc_token(TK_EQ, p);
      p += 2;
      continue;
    }

    if (*p == '!' && p[1] == '=') {
      alloc_token(TK_NE, p);
      p += 2;
      continue;
    }

    if (*p == '<' && p[1] == '=') {
      alloc_token(TK_LE, p);
      p += 2;
      continue;
    }

    if (*p == '>' && p[1] == '=') {
      alloc_token(TK_GE, p);
      p += 2;
      continue;
    }

    if (*p == '+' && p[1] == '+') {
      alloc_token(TK_INC, p);
      p += 2;
      continue;
    }

    if (*p == '-') {
      if (p[1] == '-') {
        alloc_token(TK_DEC, p);
        p += 2;
        continue;
      }
      if (p[1] == '>') {
        alloc_token(TK_ARROW, p);
        p += 2;
        continue;
      }
    }

    if (strchr("+-*/%&(){}[]<>=;,.", *p) != NULL) {
      alloc_token((enum TokenType)*p, p);
      ++p;
      continue;
    }

    if (isdigit(*p)) {
      long val = strtol(p, (char**)&p, 10);
      Token *token = alloc_token(TK_NUM, p);
      token->val = val;
      continue;
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
        alloc_token(word, p);
      } else {
        Token *token = alloc_token(TK_IDENT, p);
        token->ident = dup;
      }
      p = q;
      continue;
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

      Token *token = alloc_token(TK_CHAR, start);
      token->val = c;
      ++p;
      continue;
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
      Token *token = alloc_token(TK_STR, start);
      token->str = str;
      ++p;
      continue;
    }

    error("Unexpected character: %c\n", *p);
  }

  alloc_token(TK_EOF, p);
}

static int pos;

Token *consume(enum TokenType type) {
  Token *tok = get_token(pos);
  if (tok->type != type)
    return NULL;
  if (tok->type != TK_EOF)
    ++pos;
  return tok;
}

const char *current_line(void) {
  return get_token(pos)->input;
}
