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
  uintptr_t rodata_addr = ALIGN(start_address + sections[SEC_CODE].buf.size, 16);
  sections[SEC_RODATA].start_address = rodata_addr;

  sections[SEC_DATA].start_address = ALIGN(sections[SEC_RODATA].start_address + sections[SEC_RODATA].buf.size, 4096);
  bss_start_address = sections[SEC_DATA].start_address + ALIGN(sections[SEC_DATA].buf.size, bss_align);
}

void get_section_size(int section, size_t *pfilesz, size_t *pmemsz, uintptr_t *ploadadr) {
  switch (section) {
  case SEC_CODE:
    {
      const Section *code_sec = &sections[SEC_CODE];
      *ploadadr = code_sec->start_address;
      const Section *rodata_sec = &sections[SEC_RODATA];
      *pfilesz = *pmemsz = rodata_sec->start_address + rodata_sec->buf.size - code_sec->start_address;
    }
    break;
  case SEC_DATA:
    {
      *ploadadr = sections[SEC_DATA].start_address;
      size_t size = sections[SEC_DATA].buf.size;
      *pfilesz = size;
      *pmemsz = ALIGN(size, bss_align) + bss_size;  // Include bss.
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

  switch (section) {
  case SEC_CODE:
    {
      size_t d = sections[SEC_RODATA].start_address - (sec->start_address + sec->buf.size);
      if (d > 0) {
        char buf[16];
        assert(d < sizeof(buf));
        memset(buf, 0x00, sizeof(buf));
        fwrite(buf, 1, d, fp);
      }
      size_t rodata_size = sections[SEC_RODATA].buf.size;
      if (rodata_size > 0) {
        fwrite(sections[SEC_RODATA].buf.data, rodata_size, 1, fp);
      }
    }
    break;
  case SEC_DATA:
    break;
  default:
    assert(!"Illegal");
    break;
  }
}
