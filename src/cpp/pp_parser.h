#pragma once

#include <stdint.h>  // intptr_t
#include <stdio.h>  // FILE

#include "lexer.h"  // TokenKind, Token

typedef struct Vector Vector;

typedef intptr_t PpResult;

typedef struct {
  const char *filename;
  FILE *fp;
  int lineno;
} Stream;

Stream *set_pp_stream(Stream *stream);
PpResult pp_expr(void);
Vector *pp_funargs(void);

Token *pp_consume(enum TokenKind kind, const char *error);

void pp_parse_error(const Token *token, const char *fmt, ...);
