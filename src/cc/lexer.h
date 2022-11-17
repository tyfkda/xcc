#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>  // intptr_t
#include <stdio.h>  // FILE

#include "ast.h"  // Token, TokenKind

#define MAX_LEX_LOOKAHEAD  (2)

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
void set_source_file(FILE *fp, const char *filename);
void set_source_string(const char *line, const char *filename, int lineno);
Token *fetch_token(void);
Token *match(enum TokenKind kind);
void unget_token(Token *token);
const char *read_ident(const char *p);
Token *alloc_dummy_ident(void);
Token *alloc_token(enum TokenKind kind, const char *begin, const char *end);
const char *get_lex_p(void);
void lex_error(const char *p, const char *fmt, ...);

const char *block_comment_start(const char *p);
const char *block_comment_end(const char *p);

typedef bool (*LexEofCallback)(void);
LexEofCallback set_lex_eof_callback(LexEofCallback callback);
bool lex_eof_continue(void);
