#include "gen.h"

#include "assert.h"
#include "stdlib.h"
#include "string.h"

#include "util.h"

#define CURIP(ofs)  (instruction_pointer + ofs)
#include "x86_64.h"

#define ALIGN_SECTION_SIZE(sec, align_)  do { int align = (int)(align_); add_asm_align(align); align_section_size(sec, align); } while (0)

static Map *label_map;  // <uintptr_t adr>

enum LocType {
  LOC_REL8,
  LOC_REL32,
  LOC_ABS64,
};

typedef struct {
  enum LocType type;
  enum SectionType section;
  uintptr_t adr;
  const char *label;
  union {
    struct {
      uintptr_t base;
    } rel;
  };
} LocInfo;

typedef struct {
  uintptr_t start;
  unsigned char* buf;
  size_t size;
} Section;

static Section sections[2];
static size_t instruction_pointer;

// Put label at the current.
void add_label(const char *label) {
  map_put(label_map, label, (void*)CURIP(0));
}

void add_bss(size_t size) {
  //codesize += size;
  instruction_pointer += size;
}

static Vector *loc_vector;

static LocInfo *new_loc(enum LocType type, enum SectionType section, uintptr_t adr, const char *label) {
  LocInfo *loc = malloc(sizeof(*loc));
  loc->type = type;
  loc->section = section;
  loc->adr = adr;
  loc->label = label;
  vec_push(loc_vector, loc);
  return loc;
}

void add_loc_rel8(const char *label, int ofs, int baseofs) {
  uintptr_t adr = instruction_pointer + ofs;
  LocInfo *loc = new_loc(LOC_REL8, SEC_CODE, adr, label);
  loc->rel.base = CURIP(baseofs);
}

void add_loc_rel32(const char *label, int ofs, int baseofs) {
  uintptr_t adr = instruction_pointer + ofs;
  LocInfo *loc = new_loc(LOC_REL32, SEC_CODE, adr, label);
  loc->rel.base = CURIP(baseofs);
}

void add_loc_abs64(enum SectionType section, const char *label, uintptr_t pos) {
  new_loc(LOC_ABS64, section, pos, label);
}

uintptr_t label_adr(const char *label) {
  void *adr = map_get(label_map, label);
  return adr != NULL ? (uintptr_t)adr : (uintptr_t)-1;
}

static void add_section_data(enum SectionType secno, const unsigned char* data, size_t bytes) {
  Section *sec = &sections[secno];
  size_t size = sec->size;
  size_t newsize = size + bytes;
  unsigned char *buf = realloc(sec->buf, newsize);
  if (buf == NULL)
    error("not enough memory");
  memcpy(buf + size, data, bytes);
  sec->buf = buf;
  sec->size = newsize;
  instruction_pointer += bytes;
}

void add_code(const unsigned char* buf, size_t bytes) {
  add_section_data(SEC_CODE, buf, bytes);
}

// Resolve label locations.
void resolve_label_locations(void) {
  Vector *unsolved_labels = NULL;
  for (int i = 0; i < loc_vector->len; ++i) {
    LocInfo *loc = loc_vector->data[i];
    void *val = map_get(label_map, loc->label);
    if (val == NULL) {
      if (unsolved_labels == NULL)
        unsolved_labels = new_vector();
      bool found = false;
      for (int j = 0; j < unsolved_labels->len; ++j) {
        if (strcmp(unsolved_labels->data[j], loc->label) == 0) {
          found = true;
          break;
        }
      }
      if (!found)
        vec_push(unsolved_labels, loc->label);
      continue;
    }

    intptr_t v = (intptr_t)val;
    Section *section = &sections[loc->section];
    unsigned char *code = section->buf;
    uintptr_t offset = loc->adr - section->start;
    switch (loc->type) {
    case LOC_REL8:
      {
        intptr_t d = v - loc->rel.base;
        // TODO: Check out of range
        code[offset] = d;
      }
      break;
    case LOC_REL32:
      {
        intptr_t d = v - loc->rel.base;
        // TODO: Check out of range
        for (int i = 0; i < 4; ++i)
          code[offset + i] = d >> (i * 8);
      }
      break;
    case LOC_ABS64:
      for (int i = 0; i < 8; ++i)
        code[offset + i] = v >> (i * 8);
      break;
    default:
      assert(false);
      break;
    }
  }

  if (unsolved_labels != NULL) {
    fprintf(stderr, "Link error:\n");
    for (int i = 0; i < unsolved_labels->len; ++i)
      fprintf(stderr, "  Cannot find label `%s'\n", (char*)unsolved_labels->data[i]);
    exit(1);
  }
}

//static void dump_labels(void) {
//  add_asm_comment(NULL);
//  for (int i = 0, n = map_count(label_map); i < n; ++i) {
//    const char *name = label_map->keys->data[i];
//    uintptr_t adr = (uintptr_t)label_map->vals->data[i];
//    add_asm_comment("%08x: %s", adr, name);
//  }
//}

void get_section_size(int section, size_t *pfilesz, size_t *pmemsz, uintptr_t *ploadadr) {
  *pfilesz = sections[section].size;
  *ploadadr = sections[section].start;
  switch (section) {
  case SEC_CODE:
    *pmemsz = *pfilesz;
    break;
  case SEC_DATA:
    *pmemsz = instruction_pointer - sections[SEC_DATA].start;  // Include bss.
    break;
  default:
    assert(!"Illegal");
    break;
  }
}

void init_gen(uintptr_t start_address_) {
  sections[SEC_CODE].start = instruction_pointer = start_address_;
  label_map = new_map();
  loc_vector = new_vector();
}

void output_section(FILE* fp, int section) {
  Section *p = &sections[section];
  unsigned char *buf = p->buf;
  fwrite(buf, p->size, 1, fp);
}
