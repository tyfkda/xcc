#pragma once

#include <stdbool.h>
#include <stdint.h>  // uintptr_t, intptr_t
#include <stdio.h>  // FILE

typedef struct Vector Vector;
typedef struct Map Map;
typedef struct Node Node;

// Codegen

typedef struct {
  const char *label;
  const void *data;
  size_t size;
} RoData;

extern Vector *loc_vector;

void init_gen(uintptr_t start_address);
void gen(Node *node);
void gen_rodata(void);
void output_code(FILE* fp, size_t filesize);
void add_label(const char *label);
void add_code(const unsigned char* buf, size_t size);
void add_loc_rel8(const char *label, int ofs, int baseofs);
void add_loc_rel32(const char *label, int ofs, int baseofs);
void add_loc_abs64(const char *label, uintptr_t pos);
size_t fixup_locations(size_t *pmemsz);
uintptr_t label_adr(const char *label);
