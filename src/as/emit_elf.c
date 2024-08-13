#include "../config.h"

#if XCC_TARGET_PLATFORM != XCC_PLATFORM_APPLE
#include <assert.h>
#include <stdint.h>  // uintptr_t
#include <stdio.h>
#include <string.h>

#include "as_util.h"
#include "elfutil.h"
#include "gen_section.h"
#include "ir_asm.h"
#include "parse_asm.h"
#include "table.h"
#include "util.h"

typedef struct {
  Strtab strtab;
  Table indices;
  Elf64_Sym *buf;
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

Elf64_Sym *symtab_add(Symtab *symtab, const Name *name) {
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
  Elf64_Sym *sym = &symtab->buf[old_count];
  memset(sym, 0x00, sizeof(*sym));
  sym->st_name = offset;
  table_put(&symtab->indices, name, INT2VOIDP(old_count));
  return sym;
}

void symtab_concat(Symtab *dest, Symtab *src) {
  int n = src->count;
  const Name **names = alloca(sizeof(*names) * n);
  const Name *name;
  intptr_t index;
  for (int it = 0; (it = table_iterate(&src->indices, it, &name, (void**)&index)) != -1; ) {
    assert(index < n);
    names[index] = name;
  }
  for (int i = 0; i < n; ++i) {
    const Name *name = names[i];
    Elf64_Sym *p = symtab_add(dest, name);
    Elf64_Word st_name_bak = p->st_name;
    memcpy(p, &src->buf[i], sizeof(*p));
    p->st_name = st_name_bak;
  }
}

//

static int construct_symtab(Symtab *symtab, const int *section_indices, Table *label_table) {
  symtab_init(symtab);

  Symtab symtab_global;
  symtab_init(&symtab_global);

  Symtab *symtabs[] = {
    [STB_LOCAL] = symtab,
    [STB_GLOBAL] = &symtab_global,
  };

  const Name *nulname = alloc_name("", NULL, false);
  // UND
  Elf64_Sym *sym;
  sym = symtab_add(symtab, nulname);
  sym->st_info = ELF64_ST_INFO(STB_LOCAL, STT_NOTYPE);
  // SECTION
  for (int i = 0; i < SECTION_COUNT; ++i) {
    if (section_indices[i] <= 0)
      continue;
    sym = symtab_add(symtab, nulname);
    sym->st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
    sym->st_shndx = section_indices[i];  // Section index.
  }

  const Name *name;
  LabelInfo *info;
  for (int it = 0; (it = table_iterate(label_table, it, &name, (void**)&info)) != -1; ) {
    int bind;
    if (info->flag & LF_GLOBAL) {
      bind = STB_GLOBAL;
    } else if (info->flag & LF_REFERRED || info->kind == LK_FUNC) {
      bind = STB_LOCAL;
    } else {
      continue;
    }
    sym = symtab_add(symtabs[bind], name);
    int type;
    switch (info->kind) {
    default: assert(false);  // Fallthrough to suppress warning.
    case LK_NONE:    type = STT_NOTYPE; break;
    case LK_FUNC:    type = STT_FUNC; break;
    case LK_OBJECT:  type = STT_OBJECT; break;
    }
    sym->st_info = ELF64_ST_INFO(bind, type);
    sym->st_value = (info->flag & LF_DEFINED) ? info->address - section_start_addresses[info->section] : 0;
    assert(info->section < 0 || section_indices[info->section] > 0);
    sym->st_shndx = info->section >= 0 ? section_indices[info->section] : SHN_UNDEF;  // Symbol index for Local section.
  }

  int local_symbol_count = symtab->count;
  // Append global symbols after locals;
  symtab_concat(symtab, &symtab_global);

  return local_symbol_count;
}

static void construct_relas(Vector *unresolved, Symtab *symtab, Table *label_table,
                            const int *section_indices,
                            int rela_counts[], Elf64_Rela *rela_bufs[]) {
  memset(rela_counts, 0x00, sizeof(*rela_counts) * SECTION_COUNT);
  for (int i = 0; i < unresolved->len; ++i) {
    UnresolvedInfo *u = unresolved->data[i];
    assert(u->src_section >= 0 && u->src_section < SECTION_COUNT);
    ++rela_counts[u->src_section];
  }

  for (int i = 0; i < SECTION_COUNT; ++i) {
    int count = rela_counts[i];
    rela_bufs[i] = count <= 0 ? NULL : calloc_or_die(count * sizeof(*rela_bufs[i]));
  }
  memset(rela_counts, 0x00, sizeof(*rela_counts) * SECTION_COUNT);  // Reset count.

  for (int i = 0; i < unresolved->len; ++i) {
    UnresolvedInfo *u = unresolved->data[i];
    Elf64_Rela *rela = &rela_bufs[u->src_section][rela_counts[u->src_section]++];
    switch (u->kind) {
    case UNRES_ABS64:
      {
#if XCC_TARGET_ARCH == XCC_ARCH_RISCV64
        const int type = R_RISCV_64;
#elif XCC_TARGET_ARCH == XCC_ARCH_AARCH64
        const int type = R_AARCH64_ABS64;
#elif XCC_TARGET_ARCH == XCC_ARCH_X64
        const int type = R_X86_64_64;
#else
        assert(false);
#endif
        LabelInfo *label = table_get(label_table, u->label);
        if (label == NULL || label->flag & (LF_GLOBAL | LF_REFERRED)) {
          int symidx = symtab_find(symtab, u->label);
          assert(symidx >= 0);

          rela->r_offset = u->offset;
          rela->r_info = ELF64_R_INFO(symidx, type);
          rela->r_addend = u->add;
        } else {
          int secidx = section_indices[label->section];
          rela->r_offset = u->offset;
          rela->r_info = ELF64_R_INFO(secidx, type);
          rela->r_addend = u->add + (label->address - section_start_addresses[label->section]);
        }
      }
      break;

#if XCC_TARGET_ARCH == XCC_ARCH_X64
    case UNRES_EXTERN:
    case UNRES_EXTERN_PC32:
      {
        int symidx = symtab_find(symtab, u->label);
        assert(symidx >= 0);

        rela->r_offset = u->offset;
        rela->r_info = ELF64_R_INFO(symidx, u->kind == UNRES_EXTERN_PC32 ? R_X86_64_PC32
                                                                         : R_X86_64_PLT32);
        rela->r_addend = u->add;
      }
      break;
    case UNRES_OTHER_SECTION:
      {
        LabelInfo *label = table_get(label_table, u->label);
        assert(label != NULL);
        int secidx = section_indices[label->section];
        rela->r_offset = u->offset;
        rela->r_info = ELF64_R_INFO(secidx, R_X86_64_PC32);
        rela->r_addend = u->add;
      }
      break;

    // case UNRES_X64_GOT_LOAD:
    //   assert(!"TODO");
    //   break;

#elif XCC_TARGET_ARCH == XCC_ARCH_AARCH64
    case UNRES_CALL:
      {
        int symidx = symtab_find(symtab, u->label);
        assert(symidx >= 0);

        rela->r_offset = u->offset;
        rela->r_info = ELF64_R_INFO(symidx, R_AARCH64_CALL26);
        rela->r_addend = u->add;
      }
      break;

    case UNRES_PCREL_HI:
    case UNRES_PCREL_LO:
      {
        int symidx = symtab_find(symtab, u->label);
        assert(symidx >= 0);

        rela->r_offset = u->offset;
        rela->r_info = ELF64_R_INFO(symidx, u->kind == UNRES_PCREL_HI ? R_AARCH64_ADR_PREL_PG_HI21 : R_AARCH64_ADD_ABS_LO12_NC);
        rela->r_addend = u->add;
      }
      break;

    case UNRES_GOT_HI:
    case UNRES_GOT_LO:
      {
        LabelInfo *label = table_get(label_table, u->label);
        int type = u->kind == UNRES_GOT_HI ? R_AARCH64_ADR_GOT_PAGE : R_AARCH64_LD64_GOT_LO12_NC;
        if (label == NULL || label->flag & (LF_GLOBAL | LF_REFERRED)) {
          int symidx = symtab_find(symtab, u->label);
          assert(symidx >= 0);

          rela->r_offset = u->offset;
          rela->r_info = ELF64_R_INFO(symidx, type);
          rela->r_addend = u->add;
        } else {
          int secidx = section_indices[label->section];
          rela->r_offset = u->offset;
          rela->r_info = ELF64_R_INFO(secidx, type);
          rela->r_addend = u->add + (label->address - section_start_addresses[label->section]);
        }
      }
      break;

#elif XCC_TARGET_ARCH == XCC_ARCH_RISCV64
    case UNRES_PCREL_HI:
    case UNRES_PCREL_LO:
      {
        int symidx = symtab_find(symtab, u->label);
        assert(symidx >= 0);

        rela->r_offset = u->offset;
        rela->r_info = ELF64_R_INFO(symidx, u->kind == UNRES_PCREL_HI ? R_RISCV_PCREL_HI20 : R_RISCV_PCREL_LO12_I);
        rela->r_addend = u->add;
      }
      break;

    case UNRES_CALL:
      {
        int symidx = symtab_find(symtab, u->label);
        assert(symidx >= 0);

        rela->r_offset = u->offset;
        rela->r_info = ELF64_R_INFO(symidx, R_RISCV_CALL);
        rela->r_addend = u->add;
      }
      break;

    case UNRES_RISCV_BRANCH:
    case UNRES_RISCV_RVC_BRANCH:
      {
        Elf64_Sym *sym = symtab_add(symtab, u->label);
        size_t index = sym - symtab->buf;

        rela->r_offset = u->offset;
        rela->r_info = ELF64_R_INFO(index, u->kind == UNRES_RISCV_RVC_BRANCH ? R_RISCV_RVC_BRANCH
                                                                             : R_RISCV_BRANCH);
        rela->r_addend = u->add;
      }
      break;
    case UNRES_RISCV_JAL:
    case UNRES_RISCV_RVC_JUMP:
      {
        int symidx = symtab_find(symtab, u->label);
        assert(symidx >= 0);

        rela->r_offset = u->offset;
        rela->r_info = ELF64_R_INFO(symidx, u->kind == UNRES_RISCV_JAL ? R_RISCV_JAL : R_RISCV_RVC_JUMP);
        rela->r_addend = u->add;
      }
      break;
    case UNRES_RISCV_RELAX:
      {
        rela->r_offset = u->offset;
        rela->r_info = ELF64_R_INFO(0, R_RISCV_RELAX);
        rela->r_addend = u->add;
      }
      break;
#endif

    default: assert(false); break;
    }
  }
}

int emit_elf_obj(const char *ofn, Table *label_table, Vector *unresolved) {
  size_t section_sizes[SECTION_COUNT];
  get_section_size(SEC_CODE, &section_sizes[SEC_CODE], NULL);
  get_section_size(SEC_RODATA, &section_sizes[SEC_RODATA], NULL);
  get_section_size(SEC_DATA, &section_sizes[SEC_DATA], NULL);
  get_section_size(SEC_BSS, &section_sizes[SEC_BSS], NULL);

  int section_indices[SECTION_COUNT];
  int out_section_count = 0;
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    int index = (section_sizes[sec] > 0) ? ++out_section_count : -1;
    section_indices[sec] = index;
  }

  // Construct symtab and strtab.
  Symtab symtab;
  int local_symbol_count;
  local_symbol_count = construct_symtab(&symtab, section_indices, label_table);

  // Construct relas.
  int rela_counts[SECTION_COUNT];
  Elf64_Rela *rela_bufs[SECTION_COUNT];
  construct_relas(unresolved, &symtab, label_table, section_indices, rela_counts, rela_bufs);

  uintptr_t addr = sizeof(Elf64_Ehdr);
  uintptr_t section_offsets[SECTION_COUNT];
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    section_offsets[sec] = addr;
    if (section_sizes[sec] > 0) {
      section_offsets[sec] = addr = ALIGN(addr, 0x10);
      if (sec != SEC_BSS)
        addr += section_sizes[sec];
    }
  }

  uintptr_t rela_ofss[SECTION_COUNT];
  for (int i = 0; i < SEC_BSS; ++i) {
    rela_ofss[i] = addr = ALIGN(addr, 0x10);
    if (rela_counts[i] > 0)
      addr += sizeof(*rela_bufs[i]) * rela_counts[i];
  }

  uintptr_t symtab_ofs = addr;
  addr += sizeof(*symtab.buf) * symtab.count;
  uintptr_t strtab_ofs = addr;
  addr += symtab.strtab.size;

  // Section headers.
  Strtab shstrtab;
  strtab_init(&shstrtab);
  DataStorage section_headers;
  data_init(&section_headers);

  Elf64_Word index = 1 + out_section_count;
  for (int i = SEC_CODE; i < SEC_BSS; ++i)
    index += rela_counts[i] > 0;
  Elf64_Word strtab_index = index++;
  Elf64_Word symtab_index = index;
  int shnum = index + 2;  // symtab, shstrtab

  Elf64_Shdr nulsec = {
    .sh_name = strtab_add(&shstrtab, alloc_name("", NULL, false)),
    .sh_type = SHT_NULL,
    .sh_addralign = 1,
  };
  data_append(&section_headers, &nulsec, sizeof(nulsec));

  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    static const struct {
      const char *name;
      Elf64_Word type;
      Elf64_Xword flags;
    } kTable[] = {
      [SEC_CODE] = { .name = ".text", .type = SHT_PROGBITS, .flags = SHF_EXECINSTR | SHF_ALLOC },
      [SEC_RODATA] = { .name = ".rodata", .type = SHT_PROGBITS, .flags = SHF_ALLOC },
      [SEC_DATA] = { .name = ".data", .type = SHT_PROGBITS, .flags = SHF_WRITE | SHF_ALLOC },
      [SEC_BSS] = { .name = ".bss", .type = SHT_NOBITS, .flags = SHF_WRITE | SHF_ALLOC },
    };
    if (section_sizes[sec] > 0) {
      Elf64_Shdr shdr = {
        .sh_name = strtab_add(&shstrtab, alloc_name(kTable[sec].name, NULL, false)),
        .sh_type = kTable[sec].type,
        .sh_flags = kTable[sec].flags,
        .sh_offset = section_offsets[sec],
        .sh_size = section_sizes[sec],
        .sh_addralign = section_aligns[sec],
      };
      data_append(&section_headers, &shdr, sizeof(shdr));
    }
  }

  for (int sec = 0; sec < SEC_BSS; ++sec) {
    static const char *kNames[] = {
      [SEC_CODE] = ".rela.text",
      [SEC_RODATA] = ".rela.rodata",
      [SEC_DATA] = ".rela.data",
    };
    if (rela_counts[sec] > 0) {
      assert(section_indices[sec] > 0);
      Elf64_Shdr shdr = {
        .sh_name = strtab_add(&shstrtab, alloc_name(kNames[sec], NULL, false)),
        .sh_type = SHT_RELA,
        .sh_flags = SHF_INFO_LINK,
        .sh_offset = rela_ofss[sec],
        .sh_size = sizeof(Elf64_Rela) * rela_counts[sec],
        .sh_link = symtab_index,
        .sh_info = section_indices[sec],
        .sh_addralign = 8,
        .sh_entsize = sizeof(Elf64_Rela),
      };
      data_append(&section_headers, &shdr, sizeof(shdr));
    }
  }

  Elf64_Shdr strtabsec = {
    .sh_name = strtab_add(&shstrtab, alloc_name(".strtab", NULL, false)),
    .sh_type = SHT_STRTAB,
    .sh_offset = strtab_ofs,
    .sh_size = symtab.strtab.size,
    .sh_addralign = 1,
  };
  data_append(&section_headers, &strtabsec, sizeof(strtabsec));

  Elf64_Shdr symtabsec = {
    .sh_name = strtab_add(&shstrtab, alloc_name(".symtab", NULL, false)),
    .sh_type = SHT_SYMTAB,
    .sh_offset = symtab_ofs,
    .sh_size = sizeof(*symtab.buf) * symtab.count,
    .sh_link = strtab_index,
    .sh_info = local_symbol_count,  // Number of local symbols
    .sh_addralign = 8,
    .sh_entsize = sizeof(Elf64_Sym),
  };
  data_append(&section_headers, &symtabsec, sizeof(symtabsec));

  Elf64_Shdr shstrtabsec = {
    .sh_name = strtab_add(&shstrtab, alloc_name(".shstrtab", NULL, false)),
    .sh_type = SHT_STRTAB,
    .sh_offset = 0,  // Dummy
    .sh_size = 0,    // Dummy
    .sh_addralign = 1,
  };
  data_append(&section_headers, &shstrtabsec, sizeof(shstrtabsec));

  uintptr_t shstrtab_ofs = addr;  // ALIGN(addr, 0x10);
  addr += shstrtab.size;

  Elf64_Shdr *pshstrtabsec = (Elf64_Shdr*)(section_headers.buf + section_headers.len - sizeof(Elf64_Shdr));
  pshstrtabsec->sh_offset = shstrtab_ofs;
  pshstrtabsec->sh_size = shstrtab.size;

  uintptr_t sh_ofs = addr = ALIGN(addr, 0x10);
  // addr += section_headers.len;

  //

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

  uintptr_t entry = 0;
  int phnum = 0;
#if XCC_TARGET_ARCH == XCC_ARCH_RISCV64
  const int flags = EF_RISCV_RVC | EF_RISCV_FLOAT_ABI_DOUBLE;
#else
  const int flags = 0;
#endif
  out_elf_header(ofp, entry, phnum, shnum, flags, sh_ofs);

  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    if (section_sizes[sec] > 0) {
      put_padding(ofp, section_offsets[sec]);
      output_section(ofp, sec);
    }
  }

  for (int i = 0; i < SEC_BSS; ++i) {
    put_padding(ofp, rela_ofss[i]);
    if (rela_counts[i] > 0)
      fwrite(rela_bufs[i], sizeof(*rela_bufs[i]), rela_counts[i], ofp);
  }

  fwrite(symtab.buf, sizeof(*symtab.buf), symtab.count, ofp);
  fwrite(strtab_dump(&symtab.strtab), symtab.strtab.size, 1, ofp);

  assert(ftell(ofp) == (long)shstrtab_ofs);
  fwrite(strtab_dump(&shstrtab), shstrtab.size, 1, ofp);

  put_padding(ofp, sh_ofs);
  fwrite(section_headers.buf, section_headers.len, 1, ofp);

  return 0;
}
#endif
