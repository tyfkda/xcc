#include "../config.h"

#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
#include <assert.h>
#include <stdint.h>  // uintptr_t
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

static int construct_symtab(Symtab *symtab, Table *label_table, uintptr_t start_address) {
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

static void construct_relas(Vector *unresolved, Symtab *symtab, Table *label_table) {
  UNUSED(label_table);
  for (int i = 0; i < unresolved->len; ++i) {
    UnresolvedInfo *u = unresolved->data[i];
    SectionInfo *section = u->src_section;
    struct relocation_info *rela;
    section->rela_buf = rela = realloc_or_die(section->rela_buf,
                                              ++section->rela_count * sizeof(*rela));
    rela += section->rela_count - 1;
    switch (u->kind) {
    case UNRES_ABS64:
      {
        int symidx = symtab_find(symtab, u->label);
        assert(symidx >= 0);

        uint32_t type = 0;
#if XCC_TARGET_ARCH == XCC_ARCH_AARCH64
        type = ARM64_RELOC_UNSIGNED;
#elif XCC_TARGET_ARCH == XCC_ARCH_X64
        type = X86_64_RELOC_UNSIGNED;
#else
        assert(false);
#endif
        SET_RELOCATION_INFO(rela, u->offset, symidx, 0, 3, 1, type);
      }
      break;

#if XCC_TARGET_ARCH == XCC_ARCH_X64
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

#elif XCC_TARGET_ARCH == XCC_ARCH_AARCH64
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
          struct relocation_info *rela2;
          section->rela_buf = rela2 = realloc_or_die(section->rela_buf,
                                                     ++section->rela_count * sizeof(*rela2));
          rela2 += section->rela_count - 1;

          SET_RELOCATION_INFO(rela2, u->offset, u->add, 0, 2, 0, ARM64_RELOC_ADDEND);
        }
      }
      break;
#endif

    default: assert(false); break;
    }
  }
}

int emit_macho_obj(const char *ofn, Vector *sections, Table *label_table, Vector *unresolved) {
  int section_count = 0;
  uintptr_t start_address = 0;
  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    if ((section->ds == NULL || section->ds->len <= 0) && section->bss_size <= 0)
      continue;
    section->index = ++section_count;
    if (start_address == 0)
      start_address = section->start_address;
  }

  // Construct symtab and strtab.
  Symtab symtab;
  construct_symtab(&symtab, label_table, start_address);

  // Construct relas.
  construct_relas(unresolved, &symtab, label_table);

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

  uint32_t size_of_cmds = sizeof(struct segment_command_64) +
                          sizeof(struct section_64) * section_count +
                          sizeof(struct build_version_command) + sizeof(struct symtab_command);
  uint64_t section_start_off = sizeof(struct mach_header_64) + size_of_cmds;
  uint64_t addr = 0, off_p = section_start_off;
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
  uint64_t reloc_start_off = off_p;
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
  uint64_t symbol_start_off = off_p;
  uint64_t str_start_off = symbol_start_off + sizeof(*symtab.buf) * symtab.count;

  struct mach_header_64 header = {
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
    .sizeofcmds = size_of_cmds,
    .flags = MH_SUBSECTIONS_VIA_SYMBOLS,  // TODO: Handle this flag.
  };
  uint64_t vmsize = 0, filesize = 0;
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
  struct segment_command_64 segmentcmd = {
    .cmd = LC_SEGMENT_64,
    .cmdsize = sizeof(segmentcmd) + sizeof(struct section_64) * section_count,
    .segname = "",
    .vmaddr = 0,
    .vmsize = vmsize,
    .fileoff = section_start_off,
    .filesize = filesize,
    .maxprot = 7,   // rwx
    .initprot = 7,  // rwx
    .nsects = section_count,
    .flags = 0,
  };
  struct build_version_command buildversioncmd = {
    .cmd = LC_BUILD_VERSION,
    .cmdsize = sizeof(buildversioncmd),
    .platform = PLATFORM_MACOS,
    .minos = 0x000e0000,  // 14.0.0
    .sdk = 0x000e0500,    // 14.5.0
    .ntools = 0,
  };
  struct symtab_command symtabcmd = {
    .cmd = LC_SYMTAB,
    .cmdsize = sizeof(symtabcmd),
    .symoff = symbol_start_off,
    .nsyms = symtab.count,
    .stroff = str_start_off,
    .strsize = symtab.strtab.size,
  };

  fwrite(&header, sizeof(header), 1, ofp);
  fwrite(&segmentcmd, sizeof(segmentcmd), 1, ofp);
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
  fwrite(&buildversioncmd, sizeof(buildversioncmd), 1, ofp);
  fwrite(&symtabcmd, sizeof(symtabcmd), 1, ofp);

  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    DataStorage *ds = section->ds;
    if (ds == NULL || ds->len <= 0)
      continue;
    put_padding(ofp, section->offset);
    fwrite(ds->buf, ds->len, 1, ofp);
  }
  put_padding(ofp, reloc_start_off);
  for (int i = 0; i < sections->len; ++i) {
    SectionInfo *section = sections->data[i];
    int rela_count = section->rela_count;
    if (rela_count > 0) {
      put_padding(ofp, section->rela_ofs);
      fwrite(section->rela_buf, sizeof(struct relocation_info), rela_count, ofp);
    }
  }
  fwrite(symtab.buf, sizeof(*symtab.buf), symtab.count, ofp);
  fwrite(strtab_dump(&symtab.strtab), symtab.strtab.size, 1, ofp);

  return 0;
}
#endif
