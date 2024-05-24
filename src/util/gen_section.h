#pragma once

#include <stddef.h>  // size_t
#include <stdio.h>   // FILE
#include <stdint.h>  // uintptr_t, intptr_t

#define SECTION_COUNT  (4)

enum SectionType {
  SEC_CODE,
  SEC_RODATA,
  SEC_DATA,
  SEC_BSS,
};

extern size_t section_aligns[SECTION_COUNT];
extern uintptr_t section_start_addresses[SECTION_COUNT];

void add_code(const void *buf, size_t bytes);
void add_section_data(enum SectionType secno, const void *data, size_t bytes);
void add_bss(size_t size);
void align_section_size(enum SectionType secno, size_t align);
uintptr_t align_next_section(enum SectionType sec, uintptr_t address);

void fix_section_size(uintptr_t start_address);
void get_section_size(int section, size_t *psize, uintptr_t *ploadadr);
void output_section(FILE *fp, int section);
