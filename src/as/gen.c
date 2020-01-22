#include "gen.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "util.h"

typedef struct {
  uintptr_t start_address;
  Buffer buf;
} Section;

static Section sections[SECTION_COUNT - 1];  // -1 for BBS
static uintptr_t bss_start_address;
static size_t bss_size;
static int bss_align = 1;

void add_bss(size_t size) {
  bss_size += size;
}

void align_section_size(enum SectionType secno, int align) {
  if (secno != SEC_BSS) {
    Section *sec = &sections[secno];
    buf_align(&sec->buf, align);
  } else {
    if (align > bss_align)
      bss_align = align;
    bss_size = ALIGN(bss_size, align);
  }
}

void add_section_data(enum SectionType secno, const void *data, size_t bytes) {
  assert(secno != SEC_BSS);
  Section *sec = &sections[secno];
  buf_put(&sec->buf, data, bytes);
}

void add_code(const void *buf, size_t bytes) {
  add_section_data(SEC_CODE, buf, bytes);
}

void fix_section_size(uintptr_t start_address) {
  sections[SEC_CODE].start_address = start_address;
  sections[SEC_DATA].start_address = ALIGN(sections[SEC_CODE].start_address + sections[SEC_CODE].buf.size, 4096);
  bss_start_address = sections[SEC_DATA].start_address + ALIGN(sections[SEC_DATA].buf.size, bss_align);
}

void get_section_size(int section, size_t *pfilesz, size_t *pmemsz, uintptr_t *ploadadr) {
  size_t size = sections[section].buf.size;
  *pfilesz = size;
  *ploadadr = sections[section].start_address;
  switch (section) {
  case SEC_CODE:
    *pmemsz = size;
    break;
  case SEC_DATA:
    *pmemsz = ALIGN(size, bss_align) + bss_size;  // Include bss.
    break;
  default:
    assert(!"Illegal");
    break;
  }
}

void output_section(FILE *fp, int section) {
  Section *sec = &sections[section];
  const void *data = sec->buf.data;
  fwrite(data, sec->buf.size, 1, fp);
}
