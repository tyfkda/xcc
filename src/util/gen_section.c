#include "../config.h"
#include "gen_section.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "util.h"

typedef struct {
  uintptr_t start_address;
  DataStorage ds;
} Section;

static Section sections[SECTION_COUNT];
static size_t bss_size;

size_t section_aligns[SECTION_COUNT] = {8, 1, 8, 1};
uintptr_t section_start_addresses[SECTION_COUNT];

void add_bss(size_t size) {
  bss_size += size;
}

void align_section_size(enum SectionType secno, size_t align) {
  if (align > section_aligns[secno])
    section_aligns[secno] = align;

  if (secno != SEC_BSS) {
    Section *sec = &sections[secno];
    data_align(&sec->ds, align);
  } else {
    bss_size = ALIGN(bss_size, align);
  }
}

uintptr_t align_next_section(enum SectionType sec, uintptr_t address) {
  size_t align = section_aligns[sec];
  if (align > 1)
    address = ALIGN(address, align);
  return address;
}

void add_section_data(enum SectionType secno, const void *data, size_t bytes) {
  assert(secno != SEC_BSS);
  Section *sec = &sections[secno];
  data_append(&sec->ds, data, bytes);
}

void add_code(const void *buf, size_t bytes) {
  add_section_data(SEC_CODE, buf, bytes);
}

void fix_section_size(uintptr_t start_address) {
  uintptr_t address = start_address;
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    Section *section = &sections[sec];
    section->start_address = address = ALIGN(address, section_aligns[sec]);
    address += sec == SEC_BSS ? bss_size : section->ds.len;
  }
}

void get_section_size(int section, size_t *psize, uintptr_t *ploadadr) {
  const Section *sec = &sections[section];
  if (ploadadr != NULL)
    *ploadadr = sec->start_address;
  switch (section) {
  case SEC_CODE:
  case SEC_RODATA:
  case SEC_DATA:
    *psize = sec->ds.len;
    break;
  case SEC_BSS:
    *psize = bss_size;
    break;
  }
}

void output_section(FILE *fp, int section) {
  if (section >= SEC_BSS)
    return;
  Section *sec = &sections[section];
  const void *buf = sec->ds.buf;
  fwrite(buf, sec->ds.len, 1, fp);
}
