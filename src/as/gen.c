#include "gen.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "util.h"

typedef struct {
  uintptr_t start_address;
  Buffer buf;
} Section;

static Section sections[SECTION_COUNT];
static size_t bss_size;

size_t section_aligns[SECTION_COUNT];
uintptr_t section_start_addresses[SECTION_COUNT];

void add_bss(size_t size) {
  bss_size += size;
}

void align_section_size(enum SectionType secno, size_t align) {
  if (secno != SEC_BSS) {
    Section *sec = &sections[secno];
    buf_align(&sec->buf, align);
  } else {
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
  int rodata_align = MAX(section_aligns[SEC_RODATA], 1);
  uintptr_t rodata_addr = ALIGN(start_address + sections[SEC_CODE].buf.size, rodata_align);
  sections[SEC_RODATA].start_address = rodata_addr;

  int data_align = MAX(section_aligns[SEC_DATA], 1);
  sections[SEC_DATA].start_address = ALIGN(sections[SEC_RODATA].start_address + sections[SEC_RODATA].buf.size, data_align);
  int bss_align = MAX(section_aligns[SEC_BSS], 1);
  sections[SEC_BSS].start_address = sections[SEC_DATA].start_address + ALIGN(sections[SEC_DATA].buf.size, bss_align);
}

void get_section_size(int section, size_t *psize, uintptr_t *ploadadr) {
  switch (section) {
  case SEC_CODE:
  case SEC_RODATA:
  case SEC_DATA:
    {
      const Section *sec = &sections[section];
      if (ploadadr != NULL)
        *ploadadr = sec->start_address;
      *psize = sec->buf.size;
    }
    break;
  case SEC_BSS:
    {
      assert(ploadadr == NULL);
      *psize = bss_size;
    }
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
