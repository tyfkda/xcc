#pragma once

#include <stdio.h>  // FILE

#include "lexer.h"  // TokenKind, Token

typedef struct Macro Macro;
typedef struct Vector Vector;

typedef intptr_t PpResult;

typedef struct {
  const char *filename;
  FILE *fp;
  int lineno;
} Stream;

Stream *set_pp_stream(Stream *stream);
PpResult pp_expr(void);
Vector *pp_funargs(Vector *tokens, int *pindex, int vaarg);  // <Vector*<Token*>>

Token *pp_match(enum TokenKind kind);
Token *pp_consume(enum TokenKind kind, const char *error);

void pp_parse_error(const Token *token, const char *fmt, ...) __attribute__((noreturn));

Macro *can_expand_ident(const Name *ident);
void push_lex(const Name *ident, void (*callback)(void));
