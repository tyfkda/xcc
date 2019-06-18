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

enum SectionType {
  SEC_CODE,
  SEC_DATA,
};

void init_gen(uintptr_t start_address);
void set_asm_fp(FILE *fp);
void gen(Node *node);
void gen_rodata(void);
void output_section(FILE* fp, int section);
void add_label(const char *label);
void add_code(const unsigned char* buf, size_t size);
void add_loc_rel8(const char *label, int ofs, int baseofs);
void add_loc_rel32(const char *label, int ofs, int baseofs);
void fixup_locations(void);
void get_section_size(int section, size_t *pfilesz, size_t *pmemsz, uintptr_t *ploadadr);
uintptr_t label_adr(const char *label);
