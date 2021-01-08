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

PpResult pp_expr(void);
Vector *pp_funargs(Stream *stream);
