#pragma once

#include <stddef.h>  // size_t
#include <stdint.h>  // intptr_t
#include <stdio.h>  // FILE

// Line

typedef struct {
  const char *filename;
  int lineno;
  const char *buf;
} Line;

// Token

// Token kind
enum TokenKind {
  TK_ADD = '+',
  TK_SUB = '-',
  TK_MUL = '*',
  TK_DIV = '/',
  TK_MOD = '%',
  TK_AND = '&',
  TK_OR = '|',
  TK_HAT = '^',
  TK_LT = '<',
  TK_GT = '>',
  TK_NOT = '!',
  TK_LPAR = '(',
  TK_RPAR = ')',
  TK_LBRACE = '{',
  TK_RBRACE = '}',
  TK_LBRACKET = '[',
  TK_RBRACKET = ']',
  TK_ASSIGN = '=',
  TK_COLON = ':',
  TK_SEMICOL = ';',
  TK_COMMA = ',',
  TK_DOT = '.',
  TK_QUESTION = '?',
  TK_TILDA = '~',
  TK_INTLIT = 256,  // int literal
  TK_CHARLIT,  // char literal
  TK_LONGLIT,  // long literal
  TK_STR,        // String literal
  TK_IDENT,      // Identifier
  TK_EOF,        // Represent input end
  TK_LSHIFT,  // <<
  TK_RSHIFT,  // >>
  TK_EQ,  // ==
  TK_NE,  // !=
  TK_LE,  // <=
  TK_GE,  // >=
  TK_LOGAND,  // &&
  TK_LOGIOR,  // ||
  TK_ARROW,  // ->
  TK_ADD_ASSIGN,  // +=
  TK_SUB_ASSIGN,  // -=
  TK_MUL_ASSIGN,  // *=
  TK_DIV_ASSIGN,  // /=
  TK_MOD_ASSIGN,  // %=
  TK_AND_ASSIGN,  // &=
  TK_OR_ASSIGN,  // |=
  TK_HAT_ASSIGN,  // ^=
  TK_LSHIFT_ASSIGN,  // <<=
  TK_RSHIFT_ASSIGN,  // >>=
  TK_INC,
  TK_DEC,
  TK_IF,
  TK_ELSE,
  TK_SWITCH,
  TK_CASE,
  TK_DEFAULT,
  TK_DO,
  TK_WHILE,
  TK_FOR,
  TK_BREAK,
  TK_CONTINUE,
  TK_GOTO,
  TK_RETURN,
  TK_KWVOID,
  TK_KWCHAR,
  TK_KWSHORT,
  TK_KWINT,
  TK_KWLONG,
  TK_UNSIGNED,
  TK_KWCONST,
  TK_STATIC,
  TK_EXTERN,
  TK_STRUCT,
  TK_UNION,
  TK_ENUM,
  TK_SIZEOF,
  TK_TYPEDEF,
  TK_DOTDOTDOT,
};

// Token
typedef struct Token {
  enum TokenKind kind;
  Line *line;
  const char *begin;
  const char *end;
  union {
    const char *ident;
    struct {
      const char *buf;
      size_t size;  // Include last '\0'.
    } str;
    intptr_t value;
  };
} Token;

void init_lexer(FILE *fp, const char *filename);
void init_lexer_string(const char *line, const char *filename, int lineno);
Token *fetch_token(void);
Token *consume(enum TokenKind kind);
void unget_token(Token *token);
char *read_ident(const char **pp);
Token *alloc_ident(const char *ident, const char *begin, const char *end);
void show_error_line(const char *line, const char *p);
void parse_error(const Token *token, const char* fmt, ...);
const char *get_lex_p(void);
