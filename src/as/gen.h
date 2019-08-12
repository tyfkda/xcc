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
};

void init_gen(uintptr_t start_address);
void get_section_size(int section, size_t *pfilesz, size_t *pmemsz, uintptr_t *ploadadr);
void output_section(FILE* fp, int section);
void add_loc_rel8(const char *label, int ofs, int baseofs);
void add_loc_rel32(const char *label, int ofs, int baseofs);
uintptr_t label_adr(const char *label);

void resolve_label_locations(void);
