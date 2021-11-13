#pragma once

#include <stdint.h>  // intptr_t
#include <stdio.h>  // FILE

typedef struct Token Token;
typedef struct Vector Vector;

typedef intptr_t PpResult;

typedef struct {
  const char *filename;
  FILE *fp;
  int lineno;
} Stream;

PpResult pp_expr(void);
Vector *pp_funargs(Stream *stream);

Token *pp_consume(/*enum TokenKind*/int kind, const char *error);

void pp_parse_error(const Token *token, const char *fmt, ...);
