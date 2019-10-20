#pragma once

#include "stddef.h"
#include "stdint.h"
#include "stdio.h"

#define SECTION_COUNT  (3)

enum SectionType {
  SEC_CODE,
  SEC_DATA,
  SEC_BSS,
};

void add_code(const void* buf, size_t bytes);
void add_section_data(enum SectionType secno, const void* data, size_t bytes);
void add_bss(size_t size);
void align_section_size(enum SectionType section, int align);

void fix_section_size(uintptr_t start_address);
void get_section_size(int section, size_t *pfilesz, size_t *pmemsz, uintptr_t *ploadadr);
void output_section(FILE* fp, int section);
