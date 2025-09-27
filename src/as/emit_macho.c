#include "../config.h"

#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
#include <assert.h>
#include <stdint.h>  // uint64_t
#include <stdio.h>
#include <string.h>

#include <mach/machine.h>
#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach-o/reloc.h>

#if XCC_TARGET_ARCH == XCC_ARCH_AARCH64
#include <mach-o/arm64/reloc.h>
#elif XCC_TARGET_ARCH == XCC_ARCH_X64
#include <mach-o/x86_64/reloc.h>
#else
#error "Unsupported architecture"
#endif

#include "as_util.h"
#include "ir_asm.h"
#include "parse_asm.h"
#include "table.h"
#include "util.h"

#if !defined(__NO_BITFIELD) || !defined(__XCC)
#define SET_RELOCATION_INFO(rela, adr, sym, pcr, len, ext, typ) \
  do { \
    (rela)->r_address = (adr); \
    (rela)->r_symbolnum = (sym); \
    (rela)->r_pcrel = (pcr); \
    (rela)->r_length = (len); \
    (rela)->r_extern = (ext); \
    (rela)->r_type = (typ); \
  } while (0)
#else
#define SET_RELOCATION_INFO(rela, adr, sym, pcr, len, ext, typ) \
  do { \
    (rela)->r_address = (adr); \
    (rela)->r_pack = (((sym) & ((1U << 24) - 1U)) | (((pcr) & 1U) << 24) | (((len) & 3U) << 25) | \
                      (((ext) & 1U) << 27) | (((typ) & 0x0f) << 28));                             \
  } while (0)
#endif

typedef struct {
  Strtab strtab;
  Table indices;
  struct nlist_64 *buf;
  int count;
} Symtab;

void symtab_init(Symtab *symtab) {
  strtab_init(&symtab->strtab);
  table_init(&symtab->indices);
  symtab->buf = NULL;
  symtab->count = 0;
}

int symtab_find(Symtab *symtab, const Name *name) {
  intptr_t index;
  if (table_try_get(&symtab->indices, name, (void**)&index))
    return index;
  return -1;
}

struct nlist_64 *symtab_add(Symtab *symtab, const Name *name) {
  uint32_t offset = strtab_add(&symtab->strtab, name);
  if (name->bytes > 0) {
    int index = symtab_find(symtab, name);
    if (index >= 0)
      return &symtab->buf[index];
  }

  int old_count = symtab->count;
  int new_count = old_count + 1;
  symtab->buf = realloc_or_die(symtab->buf, sizeof(*symtab->buf) * new_count);
  symtab->count = new_count;
  struct nlist_64 *sym = &symtab->buf[old_count];
  memset(sym, 0x00, sizeof(*sym));
  sym->n_un.n_strx = offset;
  table_put(&symtab->indices, name, INT2VOIDP(old_count));
  return sym;
}

//

static int construct_symtab(Symtab *symtab, Table *label_table, uint64_t start_address) {
  symtab_init(symtab);

  // NUL
  strtab_add(&symtab->strtab, alloc_name("", NULL, false));

  const Name *name;
  LabelInfo *info;
  for (int it = 0; (it = table_iterate(label_table, it, &name, (void**)&info)) != -1; ) {
    uint8_t type = 0;
    bool sect = false;
    if (info->flag & LF_GLOBAL) {
      type = N_EXT;
      sect = (info->flag & LF_DEFINED) != 0;
    } else if (info->flag & LF_REFERRED) {
      sect = true;
    } else {
      continue;
    }
    struct nlist_64 *sym = symtab_add(symtab, name);
    if (info->section != NULL && info->flag & LF_COMM) {
      // .comm
      assert(info->align > 0 && IS_POWER_OF_2(info->align));
      SET_COMM_ALIGN(sym->n_desc, most_significant_bit(info->align));
      sym->n_value = info->size;
    } else if (sect) {
      type |= N_SECT;
      sym->n_sect = info->section->index;
      sym->n_desc = info->flag & LF_WEAK ? N_WEAK_DEF : 0;
      sym->n_value = (info->flag & LF_DEFINED) ? info->address - start_address : 0;
    }
    sym->n_type = type;
  }

  return symtab->count;
}

static inline void construct_rela_element_abs64(
    Symtab *symtab, const UnresolvedInfo *u, struct relocation_info *rela,
    uint32_t type) {
  int symidx = symtab_find(symtab, u->label);
  assert(symidx >= 0);
  SET_RELOCATION_INFO(rela, u->offset, symidx, 0, 3, 1, type);

}

#if XCC_TARGET_ARCH == XCC_ARCH_X64
static inline void construct_rela_element(
    Symtab *symtab, Table *label_table, SectionInfo *section,
    const UnresolvedInfo *u, struct relocation_info *rela) {
  UNUSED(label_table);
  UNUSED(section);
  switch (u->kind) {
  default: assert(false); break;
  case UNRES_ABS64:
    construct_rela_element_abs64(symtab, u, rela, X86_64_RELOC_UNSIGNED);
    break;

  case UNRES_EXTERN:
  case UNRES_EXTERN_PC32:
    {
      int symidx = symtab_find(symtab, u->label);
      assert(symidx >= 0);

      // assert(u->add == 0);
      rela->r_address = u->offset;
      rela->r_symbolnum = symidx;
      rela->r_pcrel = 1;
      rela->r_length = 2;
      rela->r_extern = 1;
      rela->r_type = X86_64_RELOC_BRANCH;
    }
    break;
  case UNRES_X64_GOT_LOAD:
    {
      int symidx = symtab_find(symtab, u->label);
      assert(symidx >= 0);

      // assert(u->add == 0);
      rela->r_address = u->offset;
      rela->r_symbolnum = symidx;
      rela->r_pcrel = 1;
      rela->r_length = 2;
      rela->r_extern = 1;
      rela->r_type = X86_64_RELOC_GOT_LOAD;
    }
    break;
  }
}

#elif XCC_TARGET_ARCH == XCC_ARCH_AARCH64
static inline void construct_rela_element(
    Symtab *symtab, Table *label_table, SectionInfo *section,
    const UnresolvedInfo *u, struct relocation_info *rela) {
  switch (u->kind) {
  default: assert(false); break;
  case UNRES_ABS64:
    construct_rela_element_abs64(symtab, u, rela, ARM64_RELOC_UNSIGNED);
    break;

  case UNRES_CALL:
    {
      int symidx = symtab_find(symtab, u->label);
      assert(symidx >= 0);

      assert(u->add == 0);
      SET_RELOCATION_INFO(rela, u->offset, symidx, 1, 2, 1, ARM64_RELOC_BRANCH26);
    }
    break;

  case UNRES_GOT_HI:
  case UNRES_GOT_LO:
    {
      LabelInfo *label = table_get(label_table, u->label);
      assert(label != NULL);
      int symidx = symtab_find(symtab, u->label);
      assert(symidx >= 0);

      assert(u->add == 0);
      uint32_t type = u->kind == UNRES_GOT_HI ? ARM64_RELOC_GOT_LOAD_PAGE21
                                              : ARM64_RELOC_GOT_LOAD_PAGEOFF12;
      SET_RELOCATION_INFO(rela, u->offset, symidx, u->kind == UNRES_GOT_HI ? 1 : 0, 2, 1, type);
    }
    break;

  case UNRES_PCREL_HI:
  case UNRES_PCREL_LO:
    {
      int symidx = symtab_find(symtab, u->label);
      assert(symidx >= 0);

      uint32_t type = u->kind == UNRES_PCREL_HI ? ARM64_RELOC_PAGE21 : ARM64_RELOC_PAGEOFF12;
      SET_RELOCATION_INFO(rela, u->offset, symidx, u->kind == UNRES_PCREL_HI ? 1 : 0, 2, 1, type);

      if (u->add != 0) {
        int count = ++section->rela_count;
        struct relocation_info *rela_buf;
        section->rela_buf = rela_buf = realloc_or_die(section->rela_buf, count * sizeof(*rela_buf));

        struct relocation_info *rela2 = &rela_buf[count - 1];
        SET_RELOCATION_INFO(rela2, u->offset, u->add, 0, 2, 0, ARM64_RELOC_ADDEND);
      }
    }
    break;
  }
}
#endif

static void construct_relas(Vector *unresolved, Symtab *symtab, Table *label_table) {
  for (int i = 0; i < unresolved->len; ++i) {
    UnresolvedInfo *u = unresolved->data[i];
    SectionInfo *section = u->src_section;
    int count = ++section->rela_count;
    struct relocation_info *rela_buf;
    section->rela_buf = rela_buf = realloc_or_die(section->rela_buf, count * sizeof(*rela_buf));
    construct_rela_element(symtab, label_table, section, u, &rela_buf[count - 1]);
  }
}

//

typedef struct {
  Symtab symtab;
  struct mach_header_64 header;
  struct segment_command_64 segmentcmd;
  struct build_version_command buildversioncmd;
  struct symtab_command symtabcmd;

  Vector *sections;
  uint64_t start_address;
  uint64_t reloc_start_off;
  uint64_t section_start_off;
  uint32_t size_of_cmds;
} Work;

static inline int detect_output_sections(Vector *sections, Work *work) {
  int count = 0;
  uint64_t start_address = 0;
  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    if ((section->ds == NULL || section->ds->len <= 0) && section->bss_size <= 0)
      continue;
    section->index = ++count;
    if (start_address == 0)
      start_address = section->start_address;
  }
  work->start_address = start_address;
  return count;
}

static inline void reverse_relas(Vector *sections) {
  // Reverse the order of relas.
  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    int n = section->rela_count, mid = n >> 1;
    struct relocation_info *buf = section->rela_buf;
    for (int j = 0; j < mid; ++j) {
      struct relocation_info *p = &buf[j];
      struct relocation_info *q = &buf[n - j - 1];
      struct relocation_info tmp = *p;
      *p = *q;
      *q = tmp;
    }
  }
}

static inline uint64_t arrange_section_offsets(Work *work, int section_count) {
  const uint32_t size_of_cmds =
      sizeof(struct segment_command_64) +
      sizeof(struct section_64) * section_count +
      sizeof(struct build_version_command) +
      sizeof(struct symtab_command);
  work->size_of_cmds = size_of_cmds;
  const uint64_t section_start_off = sizeof(struct mach_header_64) + size_of_cmds;
  work->section_start_off = section_start_off;
  uint64_t addr = 0, off_p = section_start_off;
  Vector *sections = work->sections;
  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    uint64_t size = section->ds != NULL ? section->ds->len : section->bss_size;
    if (size <= 0)
      continue;
    uint64_t align = section->align;
    section->start_address = addr = ALIGN(addr, align);
    off_p = ALIGN(off_p, align);
    if (section->ds != NULL) {
      section->offset = off_p;
      off_p += size;
    } else {
      section->offset = 0;
      addr += size;
    }
    addr += size;
  }
  work->reloc_start_off = off_p;
  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    if (section->ds == NULL)
      continue;
    section->rela_ofs = 0;
    int rela_count = section->rela_count;
    if (rela_count > 0) {
      section->rela_ofs = off_p;
      off_p += sizeof(struct relocation_info) * rela_count;
    }
  }

  return off_p;
}

static inline void construct_load_commands(
    Work *work, uint64_t symbol_start_off, int section_count) {
  work->header = (struct mach_header_64){
    .magic = MH_MAGIC_64,
#if XCC_TARGET_ARCH == XCC_ARCH_AARCH64
    .cputype = CPU_TYPE_ARM64,
    .cpusubtype = CPU_SUBTYPE_ARM_ALL,
#elif XCC_TARGET_ARCH == XCC_ARCH_X64
    .cputype = CPU_TYPE_X86_64,
    .cpusubtype = CPU_SUBTYPE_X86_ALL,
#else
# error "Unsupported architecture"
#endif
    .filetype = MH_OBJECT,
    .ncmds = 3,
    .sizeofcmds = work->size_of_cmds,
    .flags = MH_SUBSECTIONS_VIA_SYMBOLS,  // TODO: Handle this flag.
  };
  uint64_t vmsize = 0, filesize = 0;
  Vector *sections = work->sections;
  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    if (section->ds != NULL) {
      size_t size = section->ds->len;
      if (size <= 0)
        continue;
      vmsize = ALIGN(vmsize, section->align) + size;
      filesize = ALIGN(filesize, section->align) + size;
    } else {
      size_t size = section->bss_size;
      assert(size > 0);
      vmsize = ALIGN(vmsize, section->align) + size;
    }
  }
  work->segmentcmd = (struct segment_command_64){
    .cmd = LC_SEGMENT_64,
    .cmdsize = sizeof(work->segmentcmd) + sizeof(struct section_64) * section_count,
    .segname = "",
    .vmaddr = 0,
    .vmsize = vmsize,
    .fileoff = work->section_start_off,
    .filesize = filesize,
    .maxprot = 7,   // rwx
    .initprot = 7,  // rwx
    .nsects = section_count,
    .flags = 0,
  };
  work->buildversioncmd = (struct build_version_command){
    .cmd = LC_BUILD_VERSION,
    .cmdsize = sizeof(work->buildversioncmd),
    .platform = PLATFORM_MACOS,
    .minos = 0x000e0000,  // 14.0.0
    .sdk = 0x000e0500,    // 14.5.0
    .ntools = 0,
  };
  const uint64_t str_start_off = symbol_start_off + sizeof(*work->symtab.buf) * work->symtab.count;
  work->symtabcmd = (struct symtab_command){
    .cmd = LC_SYMTAB,
    .cmdsize = sizeof(work->symtabcmd),
    .symoff = symbol_start_off,
    .nsyms = work->symtab.count,
    .stroff = str_start_off,
    .strsize = work->symtab.strtab.size,
  };
}

static inline int output_to_file(const char *ofn, const Work *work) {
  FILE *ofp;
  if (ofn == NULL) {
    ofp = stdout;
  } else {
    ofp = fopen(ofn, "wb");
    if (ofp == NULL) {
      fprintf(stderr, "Failed to open output file: %s\n", ofn);
      return 1;
    }
  }

  fwrite(&work->header, sizeof(work->header), 1, ofp);
  fwrite(&work->segmentcmd, sizeof(work->segmentcmd), 1, ofp);
  Vector *sections = work->sections;
  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    size_t size = section->ds != NULL ? section->ds->len : section->bss_size;
    if (size <= 0)
      continue;
    uint32_t flags = 0;
    if (section->flag & SF_INIT_FUNCS)
      flags |= S_MOD_INIT_FUNC_POINTERS;
    else if (section->flag & SF_CSTRLITERALS)
      flags |= S_CSTRING_LITERALS;
    else if (section->flag & SF_BSS)
      flags |= S_ZEROFILL;
    else if (section->flag & SF_EXECUTABLE)
      flags |= S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS;
    struct section_64 sect = {
      .addr = section->start_address,
      .size = size,
      .offset = section->offset,
      .align = most_significant_bit(section->align),
      .reloff = section->rela_ofs,
      .nreloc = section->rela_count,
      .flags = flags,
    };
    strncpy(sect.sectname, section->name->chars,
            MIN(sizeof(sect.sectname), (size_t)section->name->bytes));
    if (section->segname != NULL)
      strncpy(sect.segname, section->segname, sizeof(sect.segname));
    fwrite(&sect, sizeof(sect), 1, ofp);
  }
  fwrite(&work->buildversioncmd, sizeof(work->buildversioncmd), 1, ofp);
  fwrite(&work->symtabcmd, sizeof(work->symtabcmd), 1, ofp);

  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    DataStorage *ds = section->ds;
    if (ds == NULL || ds->len <= 0)
      continue;
    put_padding(ofp, section->offset);
    fwrite(ds->buf, ds->len, 1, ofp);
  }
  put_padding(ofp, work->reloc_start_off);
  for (int i = 0; i < sections->len; ++i) {
    SectionInfo *section = sections->data[i];
    int rela_count = section->rela_count;
    if (rela_count > 0) {
      put_padding(ofp, section->rela_ofs);
      fwrite(section->rela_buf, sizeof(struct relocation_info), rela_count, ofp);
    }
  }
  fwrite(work->symtab.buf, sizeof(*work->symtab.buf), work->symtab.count, ofp);
  fwrite(strtab_dump(&work->symtab.strtab), work->symtab.strtab.size, 1, ofp);

  return 0;
}

int emit_macho_obj(const char *ofn, Vector *sections, Table *label_table, Vector *unresolved) {
  Work work;
  memset(&work, 0x00, sizeof(work));
  work.sections = sections;

  const int section_count = detect_output_sections(sections, &work);
  construct_symtab(&work.symtab, label_table, work.start_address);
  construct_relas(unresolved, &work.symtab, label_table);
  reverse_relas(sections);

  uint64_t off_p = arrange_section_offsets(&work, section_count);
  construct_load_commands(&work, off_p, section_count);
  return output_to_file(ofn, &work);
}
#endif
