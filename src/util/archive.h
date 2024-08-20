#pragma once

#include <stdint.h>
#include <stdio.h>

#include "table.h"

typedef struct Vector Vector;

typedef struct {
  void *obj;
  size_t size;
  uint32_t file_offset;
  char name[16];
} ArContent;

typedef struct {
  ArContent *content;
} ArSymbol;

typedef struct {
  FILE *fp;
  uint32_t symbol_count;
  ArSymbol *symbols;
  Table symbol_table;
  Vector *contents;  // <ArContent*>
} Archive;

Archive *load_archive(const char *filename);
void *load_archive_content(Archive *ar, ArSymbol *symbol,
                           void *(*load)(FILE*, const char*, size_t));

#define FOREACH_FILE_ARCONTENT(ar, content, body) \
  {Vector *contents = (ar)->contents; \
  for (int _ic = 0; _ic < contents->len; _ic += 1) { \
    ArContent *content = contents->data[_ic]; \
    body \
  }}
