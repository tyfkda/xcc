#include "gen.h"

#include "assert.h"
#include "stdlib.h"
#include "string.h"

#include "util.h"

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
  uintptr_t start_address;
  unsigned char* buf;
  size_t size;
} Section;

static Section sections[3];
//enum SectionType current_section;  // TODO: Use this one.
int current_section;

typedef struct {
  enum SectionType section;
  uintptr_t offset;
} LabelInfo;

// Put label at the current.
void add_label(enum SectionType section, const char *label) {
  LabelInfo *label_info = malloc(sizeof(*label_info));
  label_info->section = section;
  label_info->offset = sections[section].size;
  map_put(label_map, label, label_info);
}

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
  enum SectionType section = SEC_CODE;
  uintptr_t offset = sections[section].size;
  uintptr_t adr = offset + ofs;
  LocInfo *loc = new_loc(LOC_REL8, section, adr, label);
  loc->rel.base = offset + baseofs;
}

void add_loc_rel32(const char *label, int ofs, int baseofs) {
  enum SectionType section = SEC_CODE;
  uintptr_t offset = sections[section].size;
  uintptr_t adr = offset + ofs;
  LocInfo *loc = new_loc(LOC_REL32, section, adr, label);
  loc->rel.base = offset + baseofs;
}

void add_loc_abs64(enum SectionType section, const char *label, int ofs) {
  uintptr_t offset = sections[section].size;
  new_loc(LOC_ABS64, section, ofs + offset, label);
}

uintptr_t label_adr(const char *label) {
  LabelInfo *label_info = map_get(label_map, label);
  return label_info != NULL ? label_info->offset + sections[label_info->section].start_address : (uintptr_t)-1;
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

// Resolve label locations.
void resolve_label_locations(uintptr_t start_address) {
  sections[SEC_CODE].start_address = start_address;
  sections[SEC_DATA].start_address = ALIGN(sections[SEC_CODE].start_address + sections[SEC_CODE].size, 4096);
  sections[SEC_DATA].size = ALIGN(sections[SEC_DATA].size, 16);  // TODO: Calc max align.
  sections[SEC_BSS].start_address = sections[SEC_DATA].start_address + sections[SEC_DATA].size;

  Vector *unsolved_labels = NULL;
  for (int i = 0; i < loc_vector->len; ++i) {
    LocInfo *loc = loc_vector->data[i];
    LabelInfo *label_info = map_get(label_map, loc->label);
    if (label_info == NULL) {
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

    Section *section = &sections[loc->section];
    unsigned char *buf = section->buf;
    uintptr_t offset = loc->adr /* - section->start*/;
    uintptr_t v = label_info->offset + sections[label_info->section].start_address;
//fprintf(stderr, "  %d: %s, sec=%d, ofs=%ld, type=%d, section=%d, offset=%ld, v=%lx\n", i, loc->label, label_info->section, label_info->offset, loc->type, loc->section, offset, v);
    switch (loc->type) {
    case LOC_REL8:
      {
        intptr_t d = v - (loc->rel.base + section->start_address);
        // TODO: Check out of range
        buf[offset] = d;
      }
      break;
    case LOC_REL32:
      {
        intptr_t d = v - (loc->rel.base + section->start_address);
        // TODO: Check out of range
        for (int i = 0; i < 4; ++i)
          buf[offset + i] = d >> (i * 8);
      }
      break;
    case LOC_ABS64:
      for (int i = 0; i < 8; ++i)
        buf[offset + i] = v >> (i * 8);
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

void init_gen(void) {
  current_section = SEC_CODE;
  label_map = new_map();
  loc_vector = new_vector();
}

void output_section(FILE* fp, int section) {
  Section *p = &sections[section];
  unsigned char *buf = p->buf;
  fwrite(buf, p->size, 1, fp);
}
