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

static void putnum(FILE *fp, unsigned long num, int bytes) {
  for (int i = 0; i < bytes; ++i) {
    fputc(num, fp);
    num >>= 8;
  }
}

static int construct_symtab(Symtab *symtab, Table *label_table) {
  symtab_init(symtab);

  Symtab symtab_global;
  symtab_init(&symtab_global);

  Symtab *symtabs[] = {
    [STB_LOCAL] = symtab,
    [STB_GLOBAL] = &symtab_global,
  };

  // UND
  Elf64_Sym *sym;
  sym = symtab_add(symtab, alloc_name("", NULL, false));
  sym->st_info = ELF64_ST_INFO(STB_LOCAL, STT_NOTYPE);
  // SECTION
  for (int i = 0; i < 4; ++i) {
    sym = symtab_add(symtab, alloc_name("", NULL, false));
    sym->st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
    sym->st_shndx = i + 1;  // Section index.
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
    sym->st_shndx = info->section >= 0 ? info->section + 1 : SHN_UNDEF;  // Symbol index for Local section.
  }

  int local_symbol_count = symtab->count;
  // Append global symbols after locals;
  symtab_concat(symtab, &symtab_global);

  return local_symbol_count;
}

static void construct_relas(Vector *unresolved, Symtab *symtab, Table *label_table,
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
          rela->r_offset = u->offset;
          rela->r_info = ELF64_R_INFO(label->section + 1, type);
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
        int secidx = label->section + 1;
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
          rela->r_offset = u->offset;
          rela->r_info = ELF64_R_INFO(label->section + 1, type);
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
  // Construct symtab and strtab.
  Symtab symtab;
  int local_symbol_count;
  local_symbol_count = construct_symtab(&symtab, label_table);

  // Construct relas.
  int rela_counts[SECTION_COUNT];
  Elf64_Rela *rela_bufs[SECTION_COUNT];
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

  uintptr_t entry = 0;
  int phnum = 0;
  int shnum = 11;
#if XCC_TARGET_ARCH == XCC_ARCH_RISCV64
  const int flags = EF_RISCV_RVC | EF_RISCV_FLOAT_ABI_DOUBLE;
#else
  const int flags = 0;
#endif
  out_elf_header(ofp, entry, phnum, shnum, flags);

  size_t codesz, rodatasz, datasz, bsssz;
  get_section_size(SEC_CODE, &codesz, NULL);
  get_section_size(SEC_RODATA, &rodatasz, NULL);
  get_section_size(SEC_DATA, &datasz, NULL);
  get_section_size(SEC_BSS, &bsssz, NULL);

  uintptr_t addr = sizeof(Elf64_Ehdr);
  uintptr_t code_ofs = addr;
  output_section(ofp, SEC_CODE);
  uintptr_t rodata_ofs = addr += codesz;
  if (rodatasz > 0) {
    rodata_ofs = ALIGN(rodata_ofs, 0x10);
    put_padding(ofp, rodata_ofs);
    output_section(ofp, SEC_RODATA);
    addr = rodata_ofs + rodatasz;
  }
  uintptr_t data_ofs = addr;
  if (datasz > 0) {
    data_ofs = ALIGN(data_ofs, 0x10);
    put_padding(ofp, data_ofs);
    output_section(ofp, SEC_DATA);
    addr = data_ofs + datasz;
  }
  uintptr_t bss_ofs = addr;
  if (bsssz > 0) {
    bss_ofs = ALIGN(bss_ofs, 0x10);
    put_padding(ofp, bss_ofs);
    addr = bss_ofs;
  }

  uintptr_t rela_ofss[SECTION_COUNT];
  for (int i = 0; i < SEC_BSS; ++i) {
    rela_ofss[i] = addr = ALIGN(addr, 0x10);
    put_padding(ofp, addr);
    if (rela_counts[i] > 0) {
      fwrite(rela_bufs[i], sizeof(*rela_bufs[i]), rela_counts[i], ofp);
      addr += sizeof(*rela_bufs[i]) * rela_counts[i];
    }
  }

  uintptr_t symtab_ofs = addr + unresolved->len * sizeof(Elf64_Rela);
  put_padding(ofp, symtab_ofs);
  fwrite(symtab.buf, sizeof(*symtab.buf), symtab.count, ofp);

  uintptr_t strtab_ofs = symtab_ofs + sizeof(*symtab.buf) * symtab.count;
  fwrite(strtab_dump(&symtab.strtab), symtab.strtab.size, 1, ofp);

  // Set up shstrtab.
  Strtab shstrtab;
  strtab_init(&shstrtab);

  // Output section headers.
  {
    Elf64_Shdr nulsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name("", NULL, false)),
      .sh_type = SHT_NULL,
      .sh_addralign = 1,
    };
    Elf64_Shdr textsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".text", NULL, false)),
      .sh_type = SHT_PROGBITS,
      .sh_flags = SHF_EXECINSTR | SHF_ALLOC,
      .sh_addr = 0,
      .sh_offset = code_ofs,
      .sh_size = codesz,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = section_aligns[SEC_CODE],
      .sh_entsize = 0,
    };
    Elf64_Shdr rodatasec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".rodata", NULL, false)),
      .sh_type = SHT_PROGBITS,
      .sh_flags = SHF_ALLOC,
      .sh_addr = 0,
      .sh_offset = rodata_ofs,
      .sh_size = rodatasz,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = section_aligns[SEC_RODATA],
      .sh_entsize = 0,
    };
    Elf64_Shdr datasec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".data", NULL, false)),
      .sh_type = SHT_PROGBITS,
      .sh_flags = SHF_WRITE | SHF_ALLOC,
      .sh_addr = 0,
      .sh_offset = data_ofs,
      .sh_size = datasz,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = section_aligns[SEC_DATA],
      .sh_entsize = 0,
    };
    Elf64_Shdr bsssec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".bss", NULL, false)),
      .sh_type = SHT_NOBITS,
      .sh_flags = SHF_WRITE | SHF_ALLOC,
      .sh_addr = 0,
      .sh_offset = bss_ofs,
      .sh_size = bsssz,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = section_aligns[SEC_BSS],
      .sh_entsize = 0,
    };
    Elf64_Shdr relatextsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".rela.text", NULL, false)),
      .sh_type = SHT_RELA,
      .sh_flags = SHF_INFO_LINK,
      .sh_addr = 0,
      .sh_offset = rela_ofss[SEC_CODE],
      .sh_size = sizeof(Elf64_Rela) * rela_counts[SEC_CODE],
      .sh_link = 9,  // Index of symtab
      .sh_info = 1,  // Index of text
      .sh_addralign = 8,
      .sh_entsize = sizeof(Elf64_Rela),
    };
    Elf64_Shdr relarodatasec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".rela.rodata", NULL, false)),
      .sh_type = SHT_RELA,
      .sh_flags = SHF_INFO_LINK,
      .sh_addr = 0,
      .sh_offset = rela_ofss[SEC_RODATA],
      .sh_size = sizeof(Elf64_Rela) * rela_counts[SEC_RODATA],
      .sh_link = 9,  // Index of symtab
      .sh_info = 2,  // Index of rodata
      .sh_addralign = 8,
      .sh_entsize = sizeof(Elf64_Rela),
    };
    Elf64_Shdr reladatasec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".rela.data", NULL, false)),
      .sh_type = SHT_RELA,
      .sh_flags = SHF_INFO_LINK,
      .sh_addr = 0,
      .sh_offset = rela_ofss[SEC_DATA],
      .sh_size = sizeof(Elf64_Rela) * rela_counts[SEC_DATA],
      .sh_link = 9,  // Index of symtab
      .sh_info = 3,  // Index of data
      .sh_addralign = 8,
      .sh_entsize = sizeof(Elf64_Rela),
    };
    Elf64_Shdr strtabsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".strtab", NULL, false)),
      .sh_type = SHT_STRTAB,
      .sh_flags = 0,
      .sh_addr = 0,
      .sh_offset = strtab_ofs,
      .sh_size = symtab.strtab.size,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = 1,
      .sh_entsize = 0,
    };
    Elf64_Shdr symtabsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".symtab", NULL, false)),
      .sh_type = SHT_SYMTAB,
      .sh_flags = 0,
      .sh_addr = 0,
      .sh_offset = symtab_ofs,
      .sh_size = sizeof(*symtab.buf) * symtab.count,
      .sh_link = 8,  // Index of strtab
      .sh_info = local_symbol_count,  // Number of local symbols
      .sh_addralign = 8,
      .sh_entsize = sizeof(Elf64_Sym),
    };
    Elf64_Shdr shstrtabsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".shstrtab", NULL, false)),
      .sh_type = SHT_STRTAB,
      .sh_flags = 0,
      .sh_addr = 0,
      .sh_offset = 0,  // Dummy
      .sh_size = 0,    // Dummy
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = 1,
      .sh_entsize = 0,
    };

    long shstrtab_ofs;
    {
      void *buf = strtab_dump(&shstrtab);
      assert(buf != NULL);
      long cur = ftell(ofp);
      shstrtab_ofs = ALIGN(cur, 0x10);
      put_padding(ofp, shstrtab_ofs);
      fwrite(buf, shstrtab.size, 1, ofp);
    }
    shstrtabsec.sh_offset = shstrtab_ofs;
    shstrtabsec.sh_size = shstrtab.size;

    long cur = ftell(ofp);
    long sh_ofs = ALIGN(cur, 0x10);
    put_padding(ofp, sh_ofs);

    fwrite(&nulsec, sizeof(nulsec), 1, ofp);
    fwrite(&textsec, sizeof(textsec), 1, ofp);
    fwrite(&rodatasec, sizeof(rodatasec), 1, ofp);
    fwrite(&datasec, sizeof(datasec), 1, ofp);
    fwrite(&bsssec, sizeof(bsssec), 1, ofp);
    fwrite(&relatextsec, sizeof(relatextsec), 1, ofp);
    fwrite(&relarodatasec, sizeof(relarodatasec), 1, ofp);
    fwrite(&reladatasec, sizeof(reladatasec), 1, ofp);
    fwrite(&strtabsec, sizeof(strtabsec), 1, ofp);
    fwrite(&symtabsec, sizeof(symtabsec), 1, ofp);
    fwrite(&shstrtabsec, sizeof(shstrtabsec), 1, ofp);

    // Write section table offset.
    fseek(ofp, offsetof(Elf64_Ehdr, e_shoff), SEEK_SET);
    putnum(ofp, sh_ofs, 8);
  }

  return 0;
}
#endif
