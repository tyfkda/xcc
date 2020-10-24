#pragma once

#include <stdint.h>  // intptr_t
#include <stdio.h>  // FILE

typedef struct Vector Vector;

typedef intptr_t PpResult;

typedef struct {
  const char *filename;
  FILE *fp;
  int lineno;
} Stream;

PpResult parse_expr(void);
Vector *parse_funargs(Stream *stream);
