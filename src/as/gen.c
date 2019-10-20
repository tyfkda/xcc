#include "gen.h"

#include "assert.h"
#include "stdlib.h"
#include "string.h"

#include "util.h"

typedef struct {
  uintptr_t start_address;
  unsigned char* buf;
  size_t size;
} Section;

static Section sections[3];

typedef struct {
  enum SectionType section;
  uintptr_t offset;
} LabelInfo;

void add_bss(size_t size) {
  sections[SEC_BSS].size += size;
}

void align_section_size(enum SectionType section, int align) {
  size_t size = sections[section].size;
  size_t aligned_size = ALIGN(size, align);
  size_t add = aligned_size - size;
  if (add <= 0)
    return;

  void* zero = calloc(add, 1);
  add_section_data(section, zero, add);
  free(zero);

  assert(sections[section].size == aligned_size);
}

void add_section_data(enum SectionType secno, const void* data, size_t bytes) {
  Section *sec = &sections[secno];
  size_t size = sec->size;
  size_t newsize = size + bytes;
  unsigned char *buf = realloc(sec->buf, newsize);
  if (buf == NULL)
    error("not enough memory");
  memcpy(buf + size, data, bytes);
  sec->buf = buf;
  sec->size = newsize;
}

void add_code(const void* buf, size_t bytes) {
  add_section_data(SEC_CODE, buf, bytes);
}

void fix_section_size(uintptr_t start_address) {
  sections[SEC_CODE].start_address = start_address;
  sections[SEC_DATA].start_address = ALIGN(sections[SEC_CODE].start_address + sections[SEC_CODE].size, 4096);
  sections[SEC_DATA].size = ALIGN(sections[SEC_DATA].size, 16);  // TODO: Calc max align.
  sections[SEC_BSS].start_address = sections[SEC_DATA].start_address + sections[SEC_DATA].size;
}

void get_section_size(int section, size_t *pfilesz, size_t *pmemsz, uintptr_t *ploadadr) {
  *pfilesz = sections[section].size;
  *ploadadr = sections[section].start_address;
  switch (section) {
  case SEC_CODE:
    *pmemsz = *pfilesz;
    break;
  case SEC_DATA:
    *pmemsz = *pfilesz + sections[SEC_BSS].size;  // Include bss.
    break;
  default:
    assert(!"Illegal");
    break;
  }
}

void output_section(FILE* fp, int section) {
  Section *p = &sections[section];
  unsigned char *buf = p->buf;
  fwrite(buf, p->size, 1, fp);
}
