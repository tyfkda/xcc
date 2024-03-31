#pragma once

#include <stdint.h>
#include <stdio.h>

#include "table.h"

typedef struct Vector Vector;

typedef struct {
  uint32_t offset;
  char *ident;
} ArSymbol;

typedef struct {
  FILE *fp;
  uint32_t symbol_count;
  ArSymbol *symbols;
  Table symbol_table;
  Vector *contents;  // [i*2+0]=offset, [i*2+1]=ArContent*
} Archive;

Archive *load_archive(const char *filename);
