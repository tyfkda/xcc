#pragma once

#include <stddef.h>  // size_t
#include <stdint.h>  // uintptr_t
#include <stdio.h>   // FILE

#define SECTION_COUNT  (4)

enum SectionType {
  SEC_CODE,
  SEC_RODATA,
  SEC_DATA,
  SEC_BSS,
};

extern size_t section_aligns[SECTION_COUNT];

void add_section_data(enum SectionType secno, const void *data, size_t bytes);
void add_bss(size_t size);
void align_section_size(enum SectionType secno, size_t align);

void fix_section_size(uintptr_t start_address);
void get_section_size(int section, size_t *psize, uintptr_t *ploadadr);
void output_section(FILE *fp, int section);
