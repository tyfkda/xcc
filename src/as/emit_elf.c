#include "../config.h"

#if XCC_TARGET_PLATFORM != XCC_PLATFORM_APPLE
#include <assert.h>
#include <stdint.h>  // uint64_t
#include <stdio.h>
#include <string.h>

#include "as_util.h"
#include "elfutil.h"
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

static int construct_symtab(Symtab *symtab, Vector *sections, Table *label_table) {
  symtab_init(symtab);

  Symtab symtab_global;
  symtab_init(&symtab_global);

  Symtab symtab_weak;
  symtab_init(&symtab_weak);

  Symtab *symtabs[] = {
    [STB_LOCAL] = symtab,
    [STB_GLOBAL] = &symtab_global,
    [STB_WEAK] = &symtab_weak,
  };

  const Name *nulname = alloc_name("", NULL, false);
  // UND
  Elf64_Sym *sym;
  sym = symtab_add(symtab, nulname);
  sym->st_info = ELF64_ST_INFO(STB_LOCAL, STT_NOTYPE);
  // SECTION
  for (int i = 0; i < sections->len; ++i) {
    SectionInfo *section = sections->data[i];
    if ((section->flag & SF_BSS ? section->bss_size : section->ds->len) <= 0)
      continue;
    sym = symtab_add(symtab, nulname);
    sym->st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
    sym->st_shndx = section->index;  // Section index.
  }

  const Name *name;
  LabelInfo *info;
  for (int it = 0; (it = table_iterate(label_table, it, &name, (void**)&info)) != -1; ) {
    int bind;
    if (info->flag & LF_WEAK) {
      bind = STB_WEAK;
    } else if (info->flag & LF_GLOBAL) {
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
    sym->st_value = (info->flag & LF_DEFINED) ? info->address - info->section->start_address : 0;
    sym->st_size = info->size;
    assert(info->section == NULL || info->section->index > 0);
    Elf64_Section shndx = SHN_UNDEF;
    if (info->section != NULL) {
      if ((info->flag & (LF_COMM | LF_GLOBAL)) == (LF_COMM | LF_GLOBAL))
        shndx = SHN_COMMON;
      else
        shndx = info->section->index;  // Symbol index for Local section.
    }
    sym->st_shndx = shndx;
  }

  int local_symbol_count = symtab->count;
  // Append global symbols after locals;
  symtab_concat(symtab, &symtab_weak);
  symtab_concat(symtab, &symtab_global);

  return local_symbol_count;
}

static void construct_relas(Vector *unresolved, Symtab *symtab, Table *label_table) {
  for (int i = 0; i < unresolved->len; ++i) {
    UnresolvedInfo *u = unresolved->data[i];
    SectionInfo *section = u->src_section;
    Elf64_Rela *rela;
    section->rela_buf = rela = realloc_or_die(section->rela_buf, ++section->rela_count * sizeof(*rela));
    rela += section->rela_count - 1;
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
          int secidx = label->section->index;
          rela->r_offset = u->offset;
          rela->r_info = ELF64_R_INFO(secidx, type);
          rela->r_addend = u->add + (label->address - label->section->start_address);
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
        rela->r_info = ELF64_R_INFO(symidx, u->kind == UNRES_PCREL_HI ? R_AARCH64_ADR_PREL_PG_HI21
                                                                      : R_AARCH64_ADD_ABS_LO12_NC);
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
          int secidx = label->section->index;
          rela->r_offset = u->offset;
          rela->r_info = ELF64_R_INFO(secidx, type);
          rela->r_addend = u->add + (label->address - label->section->start_address);
        }
      }
      break;

#elif XCC_TARGET_ARCH == XCC_ARCH_RISCV64
    case UNRES_PCREL_HI:
    case UNRES_PCREL_LO:
    case UNRES_RISCV_HI20:
    case UNRES_RISCV_LO12_I:
      {
        int symidx = symtab_find(symtab, u->label);
        assert(symidx >= 0);

        Elf64_Xword type;
        switch (u->kind) {
        default: // Fallthrough to suppress warning.
        case UNRES_PCREL_HI:      type = R_RISCV_PCREL_HI20; break;
        case UNRES_PCREL_LO:      type = R_RISCV_PCREL_LO12_I; break;
        case UNRES_RISCV_HI20:    type = R_RISCV_HI20; break;
        case UNRES_RISCV_LO12_I:  type = R_RISCV_LO12_I; break;
        }

        rela->r_offset = u->offset;
        rela->r_info = ELF64_R_INFO(symidx, type);
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

#define ELF_MIN_ALIGN  0x08

int emit_elf_obj(const char *ofn, Vector *sections, Table *label_table, Vector *unresolved) {
  int out_section_count = 0;
  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    if ((section->flag & SF_BSS ? section->bss_size : section->ds->len) <= 0)
      continue;
    section->index = ++out_section_count;
  }

  // Construct symtab and strtab.
  Symtab symtab;
  int local_symbol_count;
  local_symbol_count = construct_symtab(&symtab, sections, label_table);

  // Construct relas.
  construct_relas(unresolved, &symtab, label_table);

  uint64_t addr = sizeof(Elf64_Ehdr);
  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    size_t size;
    if (section->ds == NULL || (size = section->ds->len) <= 0)
      continue;
    section->offset = addr = ALIGN(addr, ELF_MIN_ALIGN);
    addr += size;
  }

  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    if (section->rela_count <= 0)
      continue;
    section->rela_ofs = addr = ALIGN(addr, ELF_MIN_ALIGN);
    addr += sizeof(Elf64_Rela) * section->rela_count;
  }

  uint64_t symtab_ofs = addr = ALIGN(addr, ELF_MIN_ALIGN);
  addr += sizeof(*symtab.buf) * symtab.count;
  uint64_t strtab_ofs = addr;
  addr += symtab.strtab.size;

#if XCC_TARGET_ARCH == XCC_ARCH_RISCV64
  const char kRiscvAttributesArch[] = "riscv";
  const char *riscv_attributes = "rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zmmul1p0_zaamo1p0_zalrsc1p0";
  const size_t riscv_attributes_str_size = strlen(riscv_attributes) + 1;  // Include '\0'
  size_t riscv_attributes_total_size = 1 + 4 + sizeof(kRiscvAttributesArch) + 1 + 4 + 1 + riscv_attributes_str_size;
  uint64_t riscv_attributes_ofs = addr;
  addr += riscv_attributes_total_size;
#endif

  // Section headers.
  Strtab shstrtab;
  strtab_init(&shstrtab);
  DataStorage section_headers;
  data_init(&section_headers);

  Elf64_Word index = 1 + out_section_count;
  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    index += section->rela_count > 0;
  }
#if XCC_TARGET_ARCH == XCC_ARCH_RISCV64
  ++index;  // For .riscv.attributes section.
#endif
  Elf64_Word strtab_index = index++;
  Elf64_Word symtab_index = index;
  int shnum = index + 2;  // symtab, shstrtab

  Elf64_Shdr nulsec = {
    .sh_name = strtab_add(&shstrtab, alloc_name("", NULL, false)),
    .sh_type = SHT_NULL,
    .sh_addralign = 1,
  };
  data_append(&section_headers, &nulsec, sizeof(nulsec));

  const Name *init_array_name = alloc_name(".init_array", NULL, false);
  const Name *fini_array_name = alloc_name(".fini_array", NULL, false);
  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    size_t size = section->ds != NULL ? section->ds->len : section->bss_size;
    if (size <= 0)
      continue;
    Elf64_Xword flags = SHF_ALLOC;
    if (section->flag & SF_EXECUTABLE)
      flags |= SHF_EXECINSTR;
    if (section->flag & SF_WRITABLE)
      flags |= SHF_WRITE;

    Elf64_Word type;
    if (section->flag & SF_BSS)
      type = SHT_NOBITS;
    else if (equal_name(section->name, init_array_name))
      type = SHT_INIT_ARRAY;
    else if (equal_name(section->name, fini_array_name))
      type = SHT_FINI_ARRAY;
    else
      type = SHT_PROGBITS;

    Elf64_Shdr shdr = {
      .sh_name = strtab_add(&shstrtab, section->name),
      .sh_type = type,
      .sh_flags = flags,
      .sh_offset = section->offset,
      .sh_size = size,
      .sh_addralign = section->align,
    };
    data_append(&section_headers, &shdr, sizeof(shdr));
  }

  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    if (section->rela_count <= 0)
      continue;
    const Name *name = section->name;
    char *buf = malloc_or_die(name->bytes + 6);  // sizeof(".rela\0")
    snprintf(buf, name->bytes + 6, ".rela%.*s", name->bytes, name->chars);

    assert(section->index > 0);
    Elf64_Shdr shdr = {
      .sh_name = strtab_add(&shstrtab, alloc_name(buf, NULL, false)),
      .sh_type = SHT_RELA,
      .sh_flags = SHF_INFO_LINK,
      .sh_offset = section->rela_ofs,
      .sh_size = sizeof(Elf64_Rela) * section->rela_count,
      .sh_link = symtab_index,
      .sh_info = section->index,
      .sh_addralign = 8,
      .sh_entsize = sizeof(Elf64_Rela),
    };
    data_append(&section_headers, &shdr, sizeof(shdr));
  }

#if XCC_TARGET_ARCH == XCC_ARCH_RISCV64
  Elf64_Shdr riscv_attributes_sec = {
    .sh_name = strtab_add(&shstrtab, alloc_name(".riscv.attributes", NULL, false)),
    .sh_type = SHT_RISCV_ATTRIBUTES,
    .sh_offset = riscv_attributes_ofs,
    .sh_size = riscv_attributes_total_size,
    .sh_addralign = 1,
  };
  data_append(&section_headers, &riscv_attributes_sec, sizeof(riscv_attributes_sec));
#endif

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

  uint64_t shstrtab_ofs = addr;  // ALIGN(addr, 0x10);
  size_t shstrtab_name = strtab_add(&shstrtab, alloc_name(".shstrtab", NULL, false));
  Elf64_Shdr shstrtabsec = {
    .sh_name = shstrtab_name,
    .sh_type = SHT_STRTAB,
    .sh_offset = shstrtab_ofs,
    .sh_size = shstrtab.size,
    .sh_addralign = 1,
  };
  data_append(&section_headers, &shstrtabsec, sizeof(shstrtabsec));
  addr += shstrtab.size;

  uint64_t sh_ofs = addr = ALIGN(addr, 0x10);
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

  uint64_t entry = 0;
  int phnum = 0;
#if XCC_TARGET_ARCH == XCC_ARCH_RISCV64
  const int flags = EF_RISCV_RVC | EF_RISCV_FLOAT_ABI_DOUBLE;
#else
  const int flags = 0;
#endif
  out_elf_header(ofp, entry, phnum, shnum, flags, sh_ofs);

  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    DataStorage *ds = section->ds;
    if (ds == NULL || ds->len <= 0)
      continue;
    put_padding(ofp, section->offset);
    fwrite(ds->buf, ds->len, 1, ofp);
  }

  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    int rela_count = section->rela_count;
    if (rela_count > 0) {
      put_padding(ofp, section->rela_ofs);
      fwrite(section->rela_buf, sizeof(Elf64_Rela), rela_count, ofp);
    }
  }

  put_padding(ofp, symtab_ofs);
  fwrite(symtab.buf, sizeof(*symtab.buf), symtab.count, ofp);
  fwrite(strtab_dump(&symtab.strtab), symtab.strtab.size, 1, ofp);

#if XCC_TARGET_ARCH == XCC_ARCH_RISCV64
  const unsigned char RISCV_ATTRIBUTES_MAGIC = 0x41;
  fputc(RISCV_ATTRIBUTES_MAGIC, ofp);
  {
    uint32_t total_size = riscv_attributes_total_size - 1;
    fwrite(&total_size, sizeof(total_size), 1, ofp);  // TODO: Ensure little endian.
  }
  fwrite(kRiscvAttributesArch, sizeof(kRiscvAttributesArch), 1, ofp);
  fputc(0x01, ofp);  // ?
  {
    uint32_t size = 1 + 4 + 1 + riscv_attributes_str_size;
    fwrite(&size, sizeof(size), 1, ofp);  // TODO: Ensure little endian.
  }
  fputc(0x05, ofp);  // ?
  fwrite(riscv_attributes, riscv_attributes_str_size, 1, ofp);  // Output last '\0'.
#endif

  assert(ofp == stdout || ftell(ofp) == (long)shstrtab_ofs);
  fwrite(strtab_dump(&shstrtab), shstrtab.size, 1, ofp);

  put_padding(ofp, sh_ofs);
  fwrite(section_headers.buf, section_headers.len, 1, ofp);

  return 0;
}
#endif
