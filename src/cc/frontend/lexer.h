#pragma once

#include <stdbool.h>
#include <stdio.h>  // FILE

#include "ast.h"  // Token, TokenKind

#define MAX_LEX_LOOKAHEAD  (3)

typedef struct Line Line;
typedef struct Name Name;

typedef struct {
  FILE *fp;
  const char *filename;
  Line *line;
  const char *p;
  Token *fetched[MAX_LEX_LOOKAHEAD];
  int idx;
  int lineno;
} Lexer;

void init_lexer(void);
void init_lexer_for_preprocessor(void);
void set_source_file(FILE *fp, const char *filename);
void set_source_string(const char *line, const char *filename, int lineno);
Token *fetch_token(void);
Token *match(enum TokenKind kind);
void unget_token(Token *token);
const char *read_ident(const char *p);
Token *alloc_dummy_ident(void);
const char *get_lex_p(void);
_Noreturn void lex_error(const char *p, const char *fmt, ...);

typedef bool (*LexEofCallback)(void);
LexEofCallback set_lex_eof_callback(LexEofCallback callback);
bool lex_eof_continue(void);

Token *alloc_token(enum TokenKind kind, Line *line, const char *begin, const char *end);
Token *alloc_ident(const Name *name, Line *line, const char *begin, const char *end);
