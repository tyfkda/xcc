#pragma once

#include <stdint.h>
#include <stdio.h>

#include "table.h"

typedef struct ElfObj ElfObj;
typedef struct Vector Vector;

typedef struct {
  uint32_t offset;
  char *ident;
} ArSymbol;

typedef struct {
  ElfObj *elfobj;
  size_t size;
  char name[1];  // [sizeof(((struct ar_hdr*)0)->ar_name) + 1]
} ArContent;

typedef struct {
  FILE *fp;
  uint32_t symbol_count;
  ArSymbol *symbols;
  Table symbol_table;
  Vector *contents;  // [0]=offset, [1]=ArContent
} Archive;

Archive *load_archive(const char *filename);

ElfObj *load_archive_elfobj(Archive *ar, uint32_t offset);
