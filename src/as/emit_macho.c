#include "../config.h"

#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
#include <assert.h>
#include <stdint.h>  // uintptr_t
#include <stdio.h>
#include <string.h>

#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach-o/reloc.h>
#include <mach-o/arm64/reloc.h>

#include "as_util.h"
#include "gen_section.h"
#include "ir_asm.h"
#include "parse_asm.h"
#include "table.h"
#include "util.h"

#if defined(__XCC)
// MachoArm64RelocationType
#define ARM64_RELOC_UNSIGNED             0
#define ARM64_RELOC_BRANCH26             2
#define ARM64_RELOC_PAGE21               3
#define ARM64_RELOC_PAGEOFF12            4
#define ARM64_RELOC_GOT_LOAD_PAGE21      5
#define ARM64_RELOC_GOT_LOAD_PAGEOFF12   6
#define ARM64_RELOC_ADDEND               10
#endif

enum reloc_type_x86_64 {
	X86_64_RELOC_UNSIGNED,		// for absolute addresses
	X86_64_RELOC_SIGNED,		// for signed 32-bit displacement
	X86_64_RELOC_BRANCH,		// a CALL/JMP instruction with 32-bit displacement
	X86_64_RELOC_GOT_LOAD,		// a MOVQ load of a GOT entry
	X86_64_RELOC_GOT,			// other GOT references
	X86_64_RELOC_SUBTRACTOR,	// must be followed by a X86_64_RELOC_UNSIGNED
	X86_64_RELOC_SIGNED_1,		// for signed 32-bit displacement with a -1 addend
	X86_64_RELOC_SIGNED_2,		// for signed 32-bit displacement with a -2 addend
	X86_64_RELOC_SIGNED_4,		// for signed 32-bit displacement with a -4 addend
};

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

static int construct_symtab(Symtab *symtab, Table *label_table, const int section_indices[]) {
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
    if (info->section == SEC_BSS) {
      // .comm
      assert(info->align > 0 && IS_POWER_OF_2(info->align));
      SET_COMM_ALIGN(sym->n_desc, most_significant_bit(info->align));
      sym->n_value = info->size;
    } else if (sect) {
      type |= N_SECT;
      sym->n_sect = section_indices[info->section];
      sym->n_value = (info->flag & LF_DEFINED) ? info->address - section_start_addresses[0] : 0;
    }
    sym->n_type = type;
  }

  return symtab->count;
}

static void construct_relas(Vector *unresolved, Symtab *symtab, Table *label_table,
                            int rela_counts[], struct relocation_info *rela_bufs[]) {
  memset(rela_counts, 0x00, sizeof(*rela_counts) * SECTION_COUNT);
  for (int i = 0; i < unresolved->len; ++i) {
    UnresolvedInfo *u = unresolved->data[i];
    assert(u->src_section >= 0 && u->src_section < SECTION_COUNT);
    rela_counts[u->src_section] += u->add != 0 ? 2 : 1;  // TODO
  }

  for (int i = 0; i < SECTION_COUNT; ++i) {
    int count = rela_counts[i];
    rela_bufs[i] = count <= 0 ? NULL : calloc_or_die(count * sizeof(*rela_bufs[i]));
  }
  memset(rela_counts, 0x00, sizeof(*rela_counts) * SECTION_COUNT);  // Reset count.

  for (int i = 0; i < unresolved->len; ++i) {
    UnresolvedInfo *u = unresolved->data[i];
    struct relocation_info *rela = &rela_bufs[u->src_section][rela_counts[u->src_section]++];
    switch (u->kind) {
    case UNRES_EXTERN:
    case UNRES_EXTERN_PC32:
    case UNRES_OTHER_SECTION:
      {
        int symidx = symtab_find(symtab, u->label);
        assert(symidx >= 0);

        // assert(u->add == 0);
        rela->r_address = u->offset;
        rela->r_symbolnum = symidx;
        rela->r_pcrel = 1;
        rela->r_length = 2;
        rela->r_extern = 1;
        rela->r_type = u->kind == UNRES_OTHER_SECTION ? X86_64_RELOC_SIGNED : X86_64_RELOC_BRANCH;
      }
      break;
    case UNRES_ABS64:
      {
        int symidx = symtab_find(symtab, u->label);
        assert(symidx >= 0);

        rela->r_address = u->offset;
        rela->r_symbolnum = symidx;
        rela->r_pcrel = 0;
        rela->r_length = 3;
        rela->r_extern = 1;
        rela->r_type = ARM64_RELOC_UNSIGNED;
      }
      break;

    case UNRES_CALL:
      {
        int symidx = symtab_find(symtab, u->label);
        assert(symidx >= 0);

        assert(u->add == 0);
        rela->r_address = u->offset;
        rela->r_symbolnum = symidx;
        rela->r_pcrel = 1;
        rela->r_length = 2;
        rela->r_extern = 1;
        rela->r_type = ARM64_RELOC_BRANCH26;
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
        rela->r_address = u->offset;
        rela->r_symbolnum = symidx;
        rela->r_pcrel = u->kind == UNRES_GOT_HI ? 1 : 0;
        rela->r_length = 2;
        rela->r_extern = 1;
        rela->r_type = u->kind == UNRES_GOT_HI ? ARM64_RELOC_GOT_LOAD_PAGE21 : ARM64_RELOC_GOT_LOAD_PAGEOFF12;
      }
      break;

    case UNRES_PCREL_HI:
    case UNRES_PCREL_LO:
      {
#if XCC_TARGET_ARCH == XCC_ARCH_AARCH64
        int symidx = symtab_find(symtab, u->label);
        assert(symidx >= 0);

        rela->r_address = u->offset;
        rela->r_symbolnum = symidx;
        rela->r_pcrel = u->kind == UNRES_PCREL_HI ? 1 : 0;
        rela->r_length = 2;
        rela->r_extern = 1;
        rela->r_type = u->kind == UNRES_PCREL_HI ? ARM64_RELOC_PAGE21 : ARM64_RELOC_PAGEOFF12;

        if (u->add != 0) {
          struct relocation_info *rela2 = &rela_bufs[u->src_section][rela_counts[u->src_section]++];

          rela2->r_address = u->offset;
          rela2->r_symbolnum = u->add;
          rela2->r_pcrel = 0;
          rela2->r_length = 2;
          rela2->r_extern = 0;
          rela2->r_type = ARM64_RELOC_ADDEND;
        }
#else
        assert(false);
#endif
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

    default: assert(false); break;
    }
  }

  // Reverse the order of relas.
  for (int i = 0; i < SECTION_COUNT; ++i) {
    int n = rela_counts[i], mid = n >> 1;
    struct relocation_info *buf = rela_bufs[i];
    for (int j = 0; j < mid; ++j) {
      struct relocation_info *p = &buf[j];
      struct relocation_info *q = &buf[n - j - 1];
      struct relocation_info tmp = *p;
      *p = *q;
      *q = tmp;
    }
  }
}

int emit_macho_obj(const char *ofn, Table *label_table, Vector *unresolved) {
  size_t codesz, rodatasz, datasz;
  get_section_size(SEC_CODE, &codesz, NULL);
  get_section_size(SEC_RODATA, &rodatasz, NULL);
  get_section_size(SEC_DATA, &datasz, NULL);

  int section_count = 0;
  uint64_t code_reloc_off = 0, rodata_reloc_off = 0, data_reloc_off = 0;
  struct {
    size_t *psize;
    uint64_t *poff;
  } table[] = {
    {&codesz, &code_reloc_off},
    {&rodatasz, &rodata_reloc_off},
    {&datasz, &data_reloc_off},
  };

  assert(ARRAY_SIZE(table) == SEC_BSS);
  int section_indices[SEC_BSS];
  for (int i = 0; i < SEC_BSS; ++i) {
    section_indices[i] = section_count + 1;
    if (*table[i].psize > 0)
      ++section_count;
  }

  // Construct symtab and strtab.
  Symtab symtab;
  construct_symtab(&symtab, label_table, section_indices);

  // Construct relas.
  int rela_counts[SECTION_COUNT];
  struct relocation_info *rela_bufs[SECTION_COUNT];
  construct_relas(unresolved, &symtab, label_table, rela_counts, rela_bufs);

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

  uint32_t size_of_cmds = sizeof(struct segment_command_64) + sizeof(struct section_64) * section_count + sizeof(struct build_version_command) + sizeof(struct symtab_command);
  uint64_t text_start_addr = 0;
  uint64_t text_start_off = sizeof(struct mach_header_64) + size_of_cmds;
  uint64_t rodata_start_addr = ALIGN(text_start_addr + codesz, section_aligns[SEC_RODATA]);
  uint64_t rodata_start_off = ALIGN(text_start_off + codesz, section_aligns[SEC_RODATA]);
  uint64_t data_start_addr = ALIGN(rodata_start_addr + rodatasz, section_aligns[SEC_DATA]);
  uint64_t data_start_off = ALIGN(rodata_start_off + rodatasz, section_aligns[SEC_DATA]);
  uint64_t reloc_start_off = ALIGN(data_start_off + datasz, 8);
  uint64_t reloc_off_p = reloc_start_off;
  for (int i = 0; i < SEC_BSS; ++i) {
    if (rela_counts[i] > 0) {
      *table[i].poff = reloc_off_p;
      reloc_off_p += sizeof(struct relocation_info) * rela_counts[i];
    }
  }
  uint64_t symbol_start_off = reloc_off_p;
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
    .flags = MH_SUBSECTIONS_VIA_SYMBOLS,
  };
  uint64_t vmsize = 0;
  for (int i = 0; i < (int)ARRAY_SIZE(table); ++i) {
    size_t size = *table[i].psize;
    if (size > 0)
      vmsize = ALIGN(vmsize, section_aligns[i]) + size;
  }
  struct segment_command_64 segmentcmd = {
    .cmd = LC_SEGMENT_64,
    .cmdsize = sizeof(segmentcmd) + sizeof(struct section_64) * section_count,
    .segname = "",
    .vmaddr = 0,
    .vmsize = vmsize,
    .fileoff = text_start_off,
    .filesize = vmsize,
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
  if (codesz > 0) {
    struct section_64 section = {
      .sectname = "__text",
      .segname = "__TEXT",
      .addr = text_start_addr,
      .size = codesz,
      .offset = text_start_off,
      .align = most_significant_bit(section_aligns[SEC_CODE]),
      .reloff = code_reloc_off,
      .nreloc = rela_counts[SEC_CODE],
      .flags = S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS,
    };
    fwrite(&section, sizeof(section), 1, ofp);
  }
  if (rodatasz > 0) {
    struct section_64 section = {
      .sectname = "__const",
      .segname = "__DATA",
      .addr = rodata_start_addr,
      .size = rodatasz,
      .offset = rodata_start_off,
      .align = most_significant_bit(section_aligns[SEC_RODATA]),
      .reloff = rodata_reloc_off,
      .nreloc = rela_counts[SEC_RODATA],
      .flags = 0,
    };
    fwrite(&section, sizeof(section), 1, ofp);
  }
  if (datasz > 0) {
    struct section_64 section = {
      .sectname = "__data",
      .segname = "__DATA",
      .addr = data_start_addr,
      .size = datasz,
      .offset = data_start_off,
      .align = most_significant_bit(section_aligns[SEC_DATA]),
      .reloff = data_reloc_off,
      .nreloc = rela_counts[SEC_DATA],
      .flags = 0,
    };
    fwrite(&section, sizeof(section), 1, ofp);
  }
  fwrite(&buildversioncmd, sizeof(buildversioncmd), 1, ofp);
  fwrite(&symtabcmd, sizeof(symtabcmd), 1, ofp);

  output_section(ofp, SEC_CODE);
  if (rodatasz > 0) {
    put_padding(ofp, rodata_start_off);
    output_section(ofp, SEC_RODATA);
  }
  if (datasz > 0) {
    put_padding(ofp, data_start_off);
    output_section(ofp, SEC_DATA);
  }
  put_padding(ofp, reloc_start_off);
  for (int i = 0; i < SEC_BSS; ++i) {
    if (rela_counts[i] > 0)
      fwrite(rela_bufs[i], sizeof(*rela_bufs[i]), rela_counts[i], ofp);
  }
  fwrite(symtab.buf, sizeof(*symtab.buf), symtab.count, ofp);
  fwrite(strtab_dump(&symtab.strtab), symtab.strtab.size, 1, ofp);

  return 0;
}
#endif
