#pragma once

#include "stddef.h"
#include "stdint.h"
#include "stdio.h"

#ifndef ADD_LOC_REL8
#define ADD_LOC_REL8(label, ofs, base)  add_loc_rel8(label, ofs, base)
#endif
#ifndef ADD_LOC_REL32
#define ADD_LOC_REL32(label, ofs, base)  add_loc_rel32(label, ofs, base)
#endif

enum SectionType {
  SEC_CODE,
  SEC_DATA,
  SEC_BSS,
};

//extern enum SectionType current_section;
extern int current_section;

void init_gen(void);
void add_code(const void* buf, size_t bytes);
void add_section_data(enum SectionType secno, const void* data, size_t bytes);
void add_label(enum SectionType section, const char *label);
void add_bss(size_t size);
void align_section_size(enum SectionType section, int align);

void get_section_size(int section, size_t *pfilesz, size_t *pmemsz, uintptr_t *ploadadr);
void output_section(FILE* fp, int section);
void add_loc_rel8(const char *label, int ofs, int baseofs);
void add_loc_rel32(const char *label, int ofs, int baseofs);
void add_loc_abs64(enum SectionType section, const char *label, int ofs);
uintptr_t label_adr(const char *label);

void resolve_label_locations(uintptr_t start_address);
