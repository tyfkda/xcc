#include "../config.h"

#include <ar.h>
#include <assert.h>
#include <fcntl.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/stat.h>

#include "archive.h"
#include "elfobj.h"
#include "elfutil.h"
#include "table.h"
#include "util.h"

static const char kDefaultEntryName[] = "_start";

#define PROG_START      (0x100)
#define START_ADDRESS   (0x01000000 + PROG_START)
#define LOAD_ADDRESS    START_ADDRESS
#define DATA_ALIGN      (0x1000)

#define SECTION_COUNT  (2)

enum SectionType {
  SEC_TEXT,
  SEC_DATA,
};

//

typedef struct {
  const char *filename;
  enum {
    FK_ELFOBJ,
    FK_ARCHIVE,
  } kind;
  union {
    ElfObj *elfobj;
    Archive *archive;
  };
} File;

enum LinkElemKind {
  LEK_SECTION,
  LEK_SYMBOL,
  LEK_ALIGN,
};

typedef struct {
  enum LinkElemKind kind;
  union {
    struct {
      const char *name;
      Vector *list;  // <ElfSectionInfo*>
    } section;
    struct {
      uint64_t address;
    } symbol;
    Elf64_Xword align;
  };
} LinkElem;

static LinkElem *new_link_elem(enum LinkElemKind kind) {
  LinkElem *elem = calloc_or_die(sizeof(*elem));
  elem->kind = kind;
  return elem;
}

typedef struct {
  size_t align;
  uint64_t start_address;
  DataStorage *ds;
  size_t bss_size;
} SectionGroup;

typedef struct {
  File *files;
  int nfiles;
  Table *symbol_table;  // <ElfObj*>
  Table *generated_symbol_table;  // <LinkElem*>
} LinkEditor;

void ld_init(LinkEditor *ld, int nfiles) {
  ld->files = calloc_or_die(sizeof(*ld->files) * nfiles);
  ld->nfiles = nfiles;
  ld->symbol_table = alloc_table();
  assert(ld->symbol_table != NULL);
  ld->generated_symbol_table = alloc_table();
  assert(ld->generated_symbol_table != NULL);
}

void ld_load(LinkEditor *ld, int i, const char *filename) {
  char *ext = get_ext(filename);
  File *file = &ld->files[i];
  file->filename = filename;
  if (strcasecmp(ext, "o") == 0) {
    FILE *fp;
    if (!is_file(filename) || (fp = fopen(filename, "rb")) == NULL) {
      fprintf(stderr, "cannot open: %s\n", filename);
    } else {
      ElfObj *elfobj = read_elf(fp, filename);
      if (elfobj == NULL)
        exit(1);
      file->kind = FK_ELFOBJ;
      file->elfobj = elfobj;
    }
  } else if (strcasecmp(ext, "a") == 0) {
    Archive *archive = load_archive(filename);
    if (archive == NULL) {
      error("load failed: %s\n", filename);
    }
    file->kind = FK_ARCHIVE;
    file->archive = archive;
  } else {
    error("Unsupported file: %s", filename);
  }
}

static uint64_t ld_symbol_address(LinkEditor *ld, const Name *name) {
  ElfObj *elfobj = table_get(ld->symbol_table, name);
  if (elfobj != NULL) {
    Elf64_Sym *sym = elfobj_find_symbol(elfobj, name);
    assert(sym != NULL && sym->st_shndx != SHN_UNDEF);
    Elf64_Section shndx = sym->st_shndx;
    if (shndx == SHN_COMMON) {
      if (elfobj->nobit_shndx < 0) {
        // Error.
        return (uint64_t)-1;
      }
      shndx = elfobj->nobit_shndx;
    }

    if (shndx < SHN_LORESERVE) {
#ifndef NDEBUG
      const Elf64_Shdr *tshdr = &elfobj->shdrs[shndx];
      assert(tshdr->sh_type == SHT_PROGBITS || tshdr->sh_type == SHT_NOBITS);
#endif
      return elfobj->section_infos[shndx].progbits.address + sym->st_value;
    }

    assert(!"Unhandled shndx");
  } else {
    LinkElem *elem = table_get(ld->generated_symbol_table, name);
    if (elem != NULL) {
      assert(elem->kind == LEK_SYMBOL);
      return elem->symbol.address;
    }
  }
  return (uint64_t)-1;
}

// RISC-V
#define ZERO  0
#define RA    1
#define SP    2

#define IMM(imm, t, b)  (((imm) >> (b)) & ((1 << (t - b + 1)) - 1))
#define SWIZZLE_JAL(ofs)  ((IMM(ofs, 20, 20) << 31) | (IMM(ofs, 10, 1) << 21) | (IMM(ofs, 11, 11) << 20) | (IMM(ofs, 19, 12) << 12))
#define SWIZZLE_C_J(offset) \
    ((IMM(offset, 11, 11) << 12) | (IMM(offset, 4, 4) << 11) | \
     (IMM(offset, 9, 8) << 9) | (IMM(offset, 10, 10) << 8) | (IMM(offset, 6, 6) << 7) | \
     (IMM(offset, 7, 7) << 6) | (IMM(offset, 3, 1) << 3) | (IMM(offset, 5, 5) << 2))
#define SWIZZLE_C_BXX(offset) \
    ((IMM(offset, 8, 8) << 12) | (IMM(offset, 4, 3) << 10) | \
     (IMM(offset, 7, 6) << 5) | (IMM(offset, 2, 1) << 3) | (IMM(offset, 5, 5) << 2))
#define SWIZZLE_BXX(offset) \
    ((IMM(offset, 12, 12) << 31) | (IMM(offset, 10, 5) << 25) | \
     (IMM(offset, 4, 1) << 8) | (IMM(offset, 11, 11) << 7))

#define MAKE_CODE32(x)            (x)
#define ITYPE(imm, rs1, funct3, rd, opcode)          ((IMM(imm, 11, 0) << 20) | ((rs1) << 15) | ((funct3) << 12) | ((rd) << 7) | (opcode))
#define UTYPE(imm, rd, opcode)                       ((IMM(imm, 31, 12) << 12) | ((rd) << 7) | (opcode))
#define W_JAL(rd, imm)            MAKE_CODE32(UTYPE(SWIZZLE_JAL(imm), rd, 0x6f))
#define W_ADDI(rd, rs, imm)       MAKE_CODE32(ITYPE(imm, rs, 0x00, rd, 0x13))
#define P_NOP()                   W_ADDI(ZERO, ZERO, 0)

static uint64_t calc_rela_sym_address(LinkEditor *ld, ElfObj *elfobj, const Elf64_Rela *rela,
                                      const Elf64_Sym *sym, const ElfSectionInfo *strinfo) {
  uint64_t address = 0;
  switch (ELF64_ST_BIND(sym->st_info)) {
  case STB_LOCAL:
    if (ELF64_ST_TYPE(sym->st_info) == STT_SECTION) {
      assert(ELF64_R_SYM(rela->r_info) < elfobj->ehdr.e_shnum);
      const ElfSectionInfo *s = &elfobj->section_infos[ELF64_R_SYM(rela->r_info)];
      address = s->progbits.address;
    } else {
      Elf64_Section shndx = sym->st_shndx;
      if (shndx == SHN_COMMON) {
        if (elfobj->nobit_shndx < 0) {
          // Error.
          return (uint64_t)-1;
        }
        shndx = elfobj->nobit_shndx;
      }
      uint64_t sectop = elfobj->section_infos[shndx].progbits.address;
      address = sectop + sym->st_value;
    }
    break;
  case STB_GLOBAL:
  case STB_WEAK:
    {
      const char *label = &strinfo->strtab.buf[sym->st_name];
      address = ld_symbol_address(ld, alloc_name(label, NULL, false));
      assert(address != (uint64_t)-1);
    }
    break;
  default: assert(false); break;
  }

  return address + rela->r_addend;
}

typedef struct {
  LinkEditor *ld;
  ElfObj *elfobj;

  const Elf64_Shdr *shdr;
  const Elf64_Rela *relas;
  const ElfSectionInfo *symhdrinfo;
  const ElfSectionInfo *strinfo;

  uint64_t address;
  void *p;
  uint64_t pc;
} ResolveRelaWork;

#if XCC_TARGET_ARCH == XCC_ARCH_X64
static inline bool resolve_rela_element(const ResolveRelaWork *work, int j, Elf64_Xword rtype) {
  UNUSED(j);
  switch (rtype) {
  case R_X86_64_64:
    *(uint64_t*)work->p = work->address;
    break;
  case R_X86_64_PC32:
  case R_X86_64_PLT32:
    *(uint32_t*)work->p = work->address - work->pc;
    break;
  }
  return true;
}

#elif XCC_TARGET_ARCH == XCC_ARCH_AARCH64
static inline bool resolve_rela_element(const ResolveRelaWork *work, int j, Elf64_Xword rtype) {
  UNUSED(j);
  switch (rtype) {
  case R_AARCH64_ABS64:
    *(uint64_t*)work->p = work->address;
    break;
  case R_AARCH64_ADR_PREL_PG_HI21:  // Page(S+A)-Page(P)
    {
      const int PAGE = 12;
      const uint32_t MASK = ~0x60ffffe0;
      uint32_t d = (work->address >> PAGE) - (work->pc >> PAGE);
      *(uint32_t*)work->p = (*(uint32_t*)work->p & MASK) | ((d & 0x03) << 29) | ((d & 0x1ffffc) << 3);
    }
    break;
  case R_AARCH64_ADD_ABS_LO12_NC:  // S + A
    {
      const uint32_t MASK = ~(((1U << 12) - 1) << 10);
      *(uint32_t*)work->p = (*(uint32_t*)work->p & MASK) | ((work->address << 10) & ~MASK);
    }
    break;
  case R_AARCH64_CALL26:  // S+A-P
    {
      const uint32_t MASK = -(1U << 26);
      *(uint32_t*)work->p = (*(uint32_t*)work->p & MASK) | (((work->address - work->pc) >> 2) & ~MASK);
    }
    break;

  case R_AARCH64_ADR_GOT_PAGE:
  case R_AARCH64_LD64_GOT_LO12_NC:
    assert(!"TODO: Implement");
    break;
  }
  return true;
}

#elif XCC_TARGET_ARCH == XCC_ARCH_RISCV64
static inline bool resolve_rela_element(const ResolveRelaWork *work, int j, Elf64_Xword rtype) {
  switch (rtype) {
  case R_RISCV_64:
    *(uint64_t*)work->p = work->address;
    break;
  case R_RISCV_CALL:
    {
      int64_t offset = work->address - work->pc;
      assert(offset < (1L << 19) && offset >= -(1L << 19));  // TODO
      *(uint32_t*)work->p = W_JAL(RA, offset);
    }
    break;
  case R_RISCV_RELAX:
    {
      // TODO: Check
      assert(j > 0);
      const Elf64_Rela *rela0 = &work->relas[j - 1];
      switch (ELF64_R_TYPE(rela0->r_info)) {
      case R_RISCV_CALL:
        ((uint32_t*)work->p)[1] = P_NOP();
        break;
      case R_RISCV_PCREL_HI20:
      case R_RISCV_PCREL_LO12_I:
      case R_RISCV_HI20:
      case R_RISCV_LO12_I:
        break;
      default: assert(false); break;
      }
    }
    break;
  case R_RISCV_PCREL_HI20:
  case R_RISCV_HI20:
    {
      int64_t offset = work->address - (rtype == R_RISCV_PCREL_HI20 ? work->pc : 0);
      assert(offset < (1L << 31) && offset >= -(1L << 31));
      // const uint32_t MASK20 = (1U << 20) - 1;
      const uint32_t MASK12 = (1U << 12) - 1;
      if ((offset & MASK12) >= (1U << 11))
        offset += 1U << 12;
      *(uint32_t*)work->p = (*(uint32_t*)work->p & MASK12) | ((uint32_t)offset & ~MASK12);
    }
    break;
  case R_RISCV_PCREL_LO12_I:
  case R_RISCV_LO12_I:
    {
      // Get corresponding HI20 rela, and calculate the offset.
      // Assume [..., [j-2]=PCREL_HI20, [j-1]=RELAX, [j]=PCREL_LO12_I, ...]
      assert(j >= 2);
      const Elf64_Rela *hirela = &work->relas[j - 2];
      Elf64_Word hitype = ELF64_R_TYPE(hirela->r_info);
      assert((rtype == R_RISCV_PCREL_LO12_I &&
              hitype == R_RISCV_PCREL_HI20) ||
             (rtype == R_RISCV_LO12_I &&
              hitype == R_RISCV_HI20));
      const Elf64_Sym *hisym = &work->symhdrinfo->symtab.syms[ELF64_R_SYM(hirela->r_info)];
      uint64_t hiaddress = calc_rela_sym_address(work->ld, work->elfobj, hirela, hisym, work->strinfo);
      uint64_t hipc = work->elfobj->section_infos[work->shdr->sh_info].progbits.address + hirela->r_offset;

      int64_t offset = hiaddress - (rtype == R_RISCV_PCREL_LO12_I ? hipc : 0);
      assert(offset < (1L << 31) && offset >= -(1L << 31));
      const uint32_t MASK20 = (1U << 20) - 1;
      const uint32_t MASK12 = (1U << 12) - 1;
      *(uint32_t*)work->p = (*(uint32_t*)work->p & MASK20) | (((uint32_t)offset & MASK12) << 20);
    }
    break;
  case R_RISCV_RVC_JUMP:
    {
      int64_t offset = work->address - work->pc;
      assert(offset < (1L << 11) && offset >= -(1L << 11));

      uint16_t *q = (uint16_t*)work->p;
      assert((*q & 0xe003) == 0xa001);  // c.j
      *q = (*q & 0xe003) | SWIZZLE_C_J(offset);
    }
    break;
  case R_RISCV_JAL:
    {
      int64_t offset = work->address - work->pc;
      assert(offset < (1L << 19) && offset >= -(1L << 19));

      uint32_t *q = (uint32_t*)work->p;
      assert((*q & 0x0000007f) == 0x6f);  // jal
      *q = (*q & 0x0000007f) | SWIZZLE_JAL(offset);
    }
    break;
  case R_RISCV_BRANCH:
  case R_RISCV_RVC_BRANCH:
    {
      int64_t offset = work->address - work->pc;

      const Elf64_Rela *rela = &work->relas[j];
      if (ELF64_R_TYPE(rela->r_info) == R_RISCV_RVC_BRANCH) {
        assert(offset < (1 << 8) && offset >= -(1 << 8));
        // c.beqz, c.bnez
        uint16_t *q = (uint16_t*)work->p;
        assert((*q & 0xc003) == 0xc001);  // c.beqz or c.bnez
        *q = (*q & 0xe383) | SWIZZLE_C_BXX(offset);
      } else {
        assert(offset < (1 << 13) && offset >= -(1 << 13));
        uint32_t *q = (uint32_t*)work->p;
        *q = (*q & 0x01fff07f) | SWIZZLE_BXX(offset);
      }
    }
    break;
  default:
    return false;
  }
  return true;
}
#endif

static int resolve_rela_elfobj(LinkEditor *ld, ElfObj *elfobj) {
  ResolveRelaWork work;
  memset(&work, 0x00, sizeof(work));
  work.ld = ld;
  work.elfobj = elfobj;

  int error_count = 0;
  for (Elf64_Half sec = 0; sec < elfobj->ehdr.e_shnum; ++sec) {
    const Elf64_Shdr *shdr = &elfobj->shdrs[sec];
    if (shdr->sh_type != SHT_RELA || shdr->sh_size <= 0)
      continue;
    const Elf64_Shdr *symhdr = &elfobj->shdrs[shdr->sh_link];
    const ElfSectionInfo *symhdrinfo = &elfobj->section_infos[shdr->sh_link];
    const ElfSectionInfo *strinfo = &elfobj->section_infos[symhdr->sh_link];
    const ElfSectionInfo *dst_info = &elfobj->section_infos[shdr->sh_info];
    assert(dst_info->shdr->sh_type == SHT_PROGBITS || dst_info->shdr->sh_type == SHT_INIT_ARRAY ||
           dst_info->shdr->sh_type == SHT_FINI_ARRAY ||
           dst_info->shdr->sh_type == SHT_PREINIT_ARRAY);
    if (dst_info->progbits.content == NULL) {
      // Target section is not collected, so skip relocation.
      continue;
    }

    const Elf64_Rela *relas = read_or_die(elfobj->fp, NULL, shdr->sh_offset + elfobj->start_offset,
                                          shdr->sh_size, "read error");
    work.shdr = shdr;
    work.relas = relas;
    work.symhdrinfo = symhdrinfo;
    work.strinfo = strinfo;

    size_t symbol_count = elfobj->symtab_section->symtab.count;
    for (size_t j = 0, n = shdr->sh_size / sizeof(Elf64_Rela); j < n; ++j) {
      const Elf64_Rela *rela = &relas[j];
      assert(ELF64_R_SYM(rela->r_info) < symbol_count);
      const Elf64_Sym *sym = &symhdrinfo->symtab.syms[ELF64_R_SYM(rela->r_info)];
      work.address = calc_rela_sym_address(ld, elfobj, rela, sym, strinfo);

      work.p = dst_info->progbits.content + rela->r_offset;
      work.pc = elfobj->section_infos[shdr->sh_info].progbits.address + rela->r_offset;

      uint32_t rtype = ELF64_R_TYPE(rela->r_info);
      if (!resolve_rela_element(&work, j, rtype)) {
        fprintf(stderr, "Unhandled rela type: %" PRIx32 "\n", rtype);
        ++error_count;
      }
    }
  }
  return error_count;
}

static int resolve_symbols_elfobj(LinkEditor *ld, ElfObj *elfobj, Table *unresolved) {
  ElfSectionInfo *symtab_section = elfobj->symtab_section;
  assert(symtab_section != NULL);
  ElfSectionInfo *strtab_section = symtab_section->symtab.strtab;
  const char *str = strtab_section->strtab.buf;

  int error_count = 0;
  Table *defined = ld->symbol_table;
  Table *generated = ld->generated_symbol_table;
  Table *symbol_table = elfobj->symbol_table;
  const Name *name;
  Elf64_Sym *sym;
  for (int it = 0; (it = table_iterate(symbol_table, it, &name, (void**)&sym)) != -1; ) {
    if (ELF64_ST_TYPE(sym->st_info) == STT_SECTION || str[sym->st_name] == '\0')
      continue;
    const Name *name = alloc_name(&str[sym->st_name], NULL, false);

    ElfObj *prev = NULL;
    if (table_try_get(defined, name, (void**)&prev) || table_try_get(generated, name, NULL)) {
      if (sym->st_shndx != SHN_UNDEF) {
        if (prev != NULL) {
          if (ELF64_ST_BIND(sym->st_info) == STB_WEAK || sym->st_shndx == SHN_COMMON)
            continue;

          Elf64_Sym *prev_sym = table_get(prev->symbol_table, name);
          assert(prev_sym != NULL);
          if (ELF64_ST_BIND(prev_sym->st_info) == STB_WEAK || prev_sym->st_shndx == SHN_COMMON) {
            table_put(defined, name, elfobj);  // Overwrite common symbol with defined symbol.
            continue;
          }
        }
        fprintf(stderr, "Duplicate symbol: %.*s\n", NAMES(name));
        ++error_count;
      }
    } else {
      if (sym->st_shndx == SHN_UNDEF) {
        table_put(unresolved, name, (void*)name);
      } else {
        table_delete(unresolved, name);
        table_put(defined, name, elfobj);
      }
    }
  }
  return error_count;
}

static void collect_sections_elfobj(LinkEditor *ld, ElfObj *elfobj, const char *name,
                                    Vector *seclist) {
  UNUSED(ld);
  ElfSectionInfo *shsymtab = &elfobj->section_infos[elfobj->ehdr.e_shstrndx];
  assert(shsymtab->shdr->sh_type == SHT_STRTAB);
  const char *strbuf = shsymtab->strtab.buf;
  for (int i = 0; i < elfobj->prog_sections->len; ++i) {
    ElfSectionInfo *section = elfobj->prog_sections->data[i];
    if (section == NULL)
      continue;
    Elf64_Shdr *shdr = section->shdr;
    assert(shdr->sh_type == SHT_PROGBITS || shdr->sh_type == SHT_NOBITS ||
           shdr->sh_type == SHT_INIT_ARRAY || shdr->sh_type == SHT_FINI_ARRAY ||
           shdr->sh_type == SHT_PREINIT_ARRAY);
    assert(shdr->sh_size > 0);

    const char *s = &strbuf[shdr->sh_name];
    if (strcmp(s, name) == 0) {  // TODO: Wild card
      vec_push(seclist, section);
      elfobj->prog_sections->data[i] = NULL;
    }
  }
}

static void *load_elfobj(FILE *fp, const char *fn, size_t size) {
  UNUSED(size);
  return read_elf(fp, fn);
}

static int resolve_symbols_archive(LinkEditor *ld, Archive *ar, Table *unresolved) {
  int error_count = 0;
  Table *table = &ar->symbol_table;
  for (;;) {
    bool retry = false;
    const Name *name;
    for (int it = 0; (it = table_iterate(unresolved, it, &name, NULL)) != -1;) {
      ArSymbol *symbol;
      if (!table_try_get(table, name, (void**)&symbol))
        continue;

      ElfObj *elfobj = load_archive_content(ar, symbol, load_elfobj);
      if (elfobj != NULL) {
        error_count += resolve_symbols_elfobj(ld, elfobj, unresolved);
        retry = true;
        break;
      }
    }
    if (!retry)
      break;
  }
  return error_count;
}

static void collect_sections_archive(LinkEditor *ld, Archive *ar, const char *name,
                                     Vector *seclist) {
  Vector *contents = ar->contents;
  for (int i = 0; i < contents->len; ++i) {
    ArContent *content = contents->data[i];
    ElfObj *elfobj = content->obj;
    if (elfobj != NULL)
      collect_sections_elfobj(ld, elfobj, name, seclist);
  }
}

static int resolve_rela_archive(LinkEditor *ld, Archive *ar) {
  int error_count = 0;
  FOREACH_FILE_ARCONTENT(ar, content, {
    error_count += resolve_rela_elfobj(ld, content->obj);
  });
  return error_count;
}

static int ld_resolve_relas(LinkEditor *ld) {
  int error_count = 0;
  for (int i = 0; i < ld->nfiles; ++i) {
    File *file = &ld->files[i];
    switch (file->kind) {
    case FK_ELFOBJ:
      error_count += resolve_rela_elfobj(ld, file->elfobj);
      break;
    case FK_ARCHIVE:
      error_count += resolve_rela_archive(ld, file->archive);
      break;
    }
  }
  return error_count;
}

static void add_elf_section(SectionGroup section_groups[SECTION_COUNT], const ElfSectionInfo *p,
                            int secno) {
  Elf64_Shdr *shdr = p->shdr;
  if (shdr->sh_type == SHT_NOBITS)
    return;
  Elf64_Xword align = shdr->sh_addralign;
  SectionGroup *secgroup = &section_groups[secno];
  assert(align <= secgroup->align);
  assert(secgroup->ds != NULL);
  data_align(secgroup->ds, align);
  void *content = p->progbits.content;
  assert(content != NULL);
  data_append(secgroup->ds, content, shdr->sh_size);
}

static int ld_resolve_symbols(LinkEditor *ld, Table *unresolved) {
  int error_count = 0;
  for (int i = 0; i < ld->nfiles; ++i) {
    File *file = &ld->files[i];
    switch (file->kind) {
    case FK_ELFOBJ:
      error_count += resolve_symbols_elfobj(ld, file->elfobj, unresolved);
      break;
    case FK_ARCHIVE:
      error_count += resolve_symbols_archive(ld, file->archive, unresolved);
      break;
    }
  }
  return error_count;
}

static void ld_collect_sections(LinkEditor *ld, const char *name, Vector *seclist) {
  for (int i = 0; i < ld->nfiles; ++i) {
    File *file = &ld->files[i];
    switch (file->kind) {
    case FK_ELFOBJ:
      collect_sections_elfobj(ld, file->elfobj, name, seclist);
      break;
    case FK_ARCHIVE:
      collect_sections_archive(ld, file->archive, name, seclist);
      break;
    }
  }
}

static void ld_calc_address(SectionGroup section_groups[SECTION_COUNT],
                            Vector *section_lists[SECTION_COUNT], uint64_t start_address) {
  uint64_t address = start_address;
  for (int secno = 0; secno < SECTION_COUNT; ++secno) {
    Vector *v = section_lists[secno];
    if (v->len <= 0)
      continue;

    SectionGroup *secgroup = &section_groups[secno];
    secgroup->start_address = address = ALIGN(address, secgroup->align);
    for (int i = 0; i < v->len; ++i) {
      LinkElem *elem = v->data[i];
      switch (elem->kind) {
      case LEK_SECTION:
        {
          Vector *list = elem->section.list;
          for (int j = 0; j < list->len; ++j) {
            ElfSectionInfo *p = list->data[j];
            Elf64_Shdr *shdr = p->shdr;
            size_t align = shdr->sh_addralign;
            address = ALIGN(address, align);
            p->progbits.address = address;
            size_t size = shdr->sh_size;
            address += size;

            if (align > secgroup->align)
              secgroup->align = align;

            if (p->shdr->sh_type == SHT_NOBITS)
              secgroup->bss_size += size;
          }
        }
        break;
      case LEK_SYMBOL:
        elem->symbol.address = address;
        break;
      case LEK_ALIGN:
        address = ALIGN(address, elem->align);
        break;
      }
    }
  }
}

static void ld_load_elf_objects(Vector *section_lists[SECTION_COUNT]) {
  for (int secno = 0; secno < SECTION_COUNT; ++secno) {
    Vector *v = section_lists[secno];
    if (v->len <= 0)
      continue;

    for (int i = 0; i < v->len; ++i) {
      LinkElem *elem = v->data[i];
      switch (elem->kind) {
      case LEK_SECTION:
        {
          Vector *list = elem->section.list;
          for (int j = 0; j < list->len; ++j) {
            ElfSectionInfo *p = list->data[j];
            Elf64_Shdr *shdr = p->shdr;
            switch (shdr->sh_type) {
            case SHT_PROGBITS:
            case SHT_INIT_ARRAY:
            case SHT_FINI_ARRAY:
            case SHT_PREINIT_ARRAY:
              {
                Elf64_Xword size = shdr->sh_size;
                assert(size > 0);

                ElfObj *elfobj = p->elfobj;
                void *buf = read_or_die(elfobj->fp, NULL, shdr->sh_offset + elfobj->start_offset,
                                        size, "read error");
                p->progbits.content = buf;
              }
              break;
            default: break;
            }
          }
        }
        break;
      case LEK_SYMBOL:
      case LEK_ALIGN:
        break;
      }
    }
  }
}

static void output_section(FILE *fp, SectionGroup *secgroup) {
  if (secgroup->ds == NULL)
    return;
  DataStorage *ds = secgroup->ds;
  const void *buf = ds->buf;
  fwrite(buf, ds->len, 1, fp);
}

static bool output_exe(const char *ofn, uint64_t entry_address,
                       SectionGroup section_groups[SECTION_COUNT]) {
  int phnum = section_groups[SEC_DATA].ds->len > 0 || section_groups[SEC_DATA].bss_size > 0 ? 2 : 1;

  FILE *fp;
  if (ofn == NULL) {
    fp = stdout;
  } else {
    const int mod = S_IRUSR | S_IWUSR | S_IXUSR | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH;  // 0755
    const int flag = O_WRONLY | O_CREAT | O_TRUNC;
    int fd = open(ofn, flag, mod);
    if (fd < 0) {
      perror("open failed");
      return false;
    }
    fp = fdopen(fd, "wb");
    assert(fp != NULL);
  }

  size_t code_rodata_sz = section_groups[SEC_TEXT].ds->len;
#if XCC_TARGET_ARCH == XCC_ARCH_RISCV64
  const int flags = EF_RISCV_RVC | EF_RISCV_FLOAT_ABI_DOUBLE;
#else
  const int flags = 0;
#endif
  out_elf_header(fp, entry_address, phnum, 0, flags, 0);
  out_program_header(fp, 0, PROG_START, section_groups[SEC_TEXT].start_address, code_rodata_sz,
                     code_rodata_sz);
  if (phnum > 1) {
    size_t datamemsz = section_groups[SEC_DATA].ds->len + section_groups[SEC_DATA].bss_size;
    uint64_t offset = PROG_START + code_rodata_sz;
    if (section_groups[SEC_DATA].ds->len > 0)
      offset = ALIGN(offset, DATA_ALIGN);
    out_program_header(fp, 1, offset, section_groups[SEC_DATA].start_address,
                       section_groups[SEC_DATA].ds->len, datamemsz);
  }

  uint64_t addr = PROG_START;
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    addr = ALIGN(addr, section_groups[sec].align);
    size_t size = section_groups[sec].ds->len;
    if (size <= 0)
      continue;
    put_padding(fp, addr);
    output_section(fp, &section_groups[sec]);
    addr += size;
  }
  fclose(fp);

  return true;
}

#define DSF_WEAK       (1 << 0)
#define DSF_LOCAL      (1 << 1)
#define DSF_GENERATED  (1 << 2)

typedef struct {
  const char *filename;
  const char *name;
  uint64_t address;
  int flag;
} DumpSymbol;

static void dump_map_elfobj(LinkEditor *ld, ElfObj *elfobj, File *file, ArContent *content,
                            Vector *symbols) {
  ElfSectionInfo *symtab = elfobj->symtab_section;
  assert(symtab != NULL);
  const char *strbuf = symtab->symtab.strtab->strtab.buf;
  for (size_t i = 0; i < symtab->symtab.count; ++i) {
    Elf64_Sym *sym = &symtab->symtab.syms[i];
    if (sym->st_shndx == SHN_UNDEF)
      continue;

    const char *name = &strbuf[sym->st_name];
    uint64_t address = 0;
    int flag = 0;
    unsigned char bind = ELF64_ST_BIND(sym->st_info);
    switch (bind) {
    case STB_LOCAL:
    case STB_WEAK:
      {
        Elf64_Word type = ELF64_ST_TYPE(sym->st_info);
        if (type != STT_FUNC && type != STT_OBJECT) {
          continue;
        }
        const ElfSectionInfo *s = &elfobj->section_infos[sym->st_shndx];
        address = s->progbits.address + sym->st_value;
        flag |= bind == STB_WEAK ? DSF_WEAK : DSF_LOCAL;
      }
      break;
    case STB_GLOBAL:
      address = ld_symbol_address(ld, alloc_name(name, NULL, false));
      break;
    default: assert(false); break;
    }

    DumpSymbol *ds = calloc_or_die(sizeof(*ds));
    ds->filename = content != NULL ? content->name : file->filename;  // TODO: Confirm nul-terminated.
    ds->name = name;
    ds->address = address;
    ds->flag = flag;
    vec_push(symbols, ds);
  }
}

static void dump_map_file(LinkEditor *ld, Vector *symbols) {
  LinkElem *elem;
  const Name *name;
  for (int it = 0;
       (it = table_iterate(ld->generated_symbol_table, it, &name, (void **)&elem)) != -1;) {
    assert(elem->kind == LEK_SYMBOL);
    uint64_t address = elem->symbol.address;
    DumpSymbol *ds = calloc_or_die(sizeof(*ds));
    ds->filename = "*generated*";
    ds->name = name->chars;  // TODO: Confirm nul-terminated.
    ds->address = address;
    ds->flag = DSF_GENERATED;
    vec_push(symbols, ds);
  }

  for (int i = 0; i < ld->nfiles; ++i) {
    File *file = &ld->files[i];
    switch (file->kind) {
    case FK_ELFOBJ:
      dump_map_elfobj(ld, file->elfobj, file, NULL, symbols);
      break;
    case FK_ARCHIVE:
      {
        Archive *ar = file->archive;
        FOREACH_FILE_ARCONTENT(ar, content, {
          dump_map_elfobj(ld, content->obj, file, content, symbols);
        });
      }
      break;
    }
  }
}

static int sort_dump_symbol(const void *a, const void *b) {
  DumpSymbol *dsa = *(DumpSymbol**)a;
  DumpSymbol *dsb = *(DumpSymbol**)b;
  if (dsa->address != dsb->address)
    return dsa->address < dsb->address ? -1 : 1;
  return a < b ? -1 : 1;
}

static bool output_map_file(LinkEditor *ld, const char *outmapfn, uint64_t entry_address,
                            const Name *entry_name) {
  Vector *symbols = new_vector();  // <DumpSymbol*>
  dump_map_file(ld, symbols);
  qsort(symbols->data, symbols->len, sizeof(*symbols->data), sort_dump_symbol);

  FILE *mapfp;
  if (strcmp(outmapfn, "-") == 0) {
    mapfp = stdout;
  } else {
    mapfp = fopen(outmapfn, "w");
    if (mapfp == NULL) {
      perror("Failed to open map file");
      return false;
    }
  }

  fprintf(mapfp, "### Symbols\n");
  fprintf(mapfp, "%9lx:  (start address)\n", (long)LOAD_ADDRESS);

  for (int i = 0; i < symbols->len; ++i) {
    DumpSymbol *ds = symbols->data[i];
    static const char kFlagChars[] = {
      [DSF_WEAK] = 'w',
      [DSF_LOCAL] = 'L',
    };
    char fc = kFlagChars[ds->flag & (DSF_WEAK | DSF_LOCAL)];
    if (fc == '\0')
      fc = 'G';
    fprintf(mapfp, "%9" PRIx64 ": %c %s  (%s)\n", ds->address, fc, ds->name, ds->filename);
  }

  fprintf(mapfp, "\n### Entry point\n");
  fprintf(mapfp, "%9" PRIx64 ": %.*s\n", entry_address, NAMES(entry_name));

  if (mapfp != stdout)
    fclose(mapfp);

  return true;
}

// search 'libXXX.a' from library paths.
static const char *search_library(Vector *lib_paths, const char *libname) {
  char libfn[128];  // TODO: Avoid overflow.
  snprintf(libfn, sizeof(libfn), "lib%s.a", libname);

  for (int i = 0; i < lib_paths->len; ++i) {
    const char *dir = lib_paths->data[i];
    const char *path = JOIN_PATHS(dir, libfn);
    if (is_file(path)) {
      return path;
    }
  }
  return NULL;
}

static void usage(FILE *fp) {
  fprintf(
      fp,
      "Usage: ld [options] file...\n"
      "Options:\n"
      "  -o <filename>       Set output filename (Default: Standard output)\n"
      "  -l <name>           Add library\n"
      "  -L <path>           Add library path\n"
      "  -e <funcname>       Set entry function (Default: _start)\n"
  );
}

typedef struct {
  const char *ofn;
  const char *entry;
  const char *outmapfn;
} Options;

static Vector *parse_options(int argc, char *argv[], Options *opts) {
  enum {
    OPT_HELP = 128,
    OPT_VERSION,
    OPT_OUTMAP,

    OPT_NO_PIE,
  };

  static const struct option kOptions[] = {
    {"o", required_argument},  // Specify output filename
    {"e", required_argument},  // Entry name
    {"l", required_argument},  // Library
    {"L", required_argument},  // Add library path
    {"Map", required_argument, OPT_OUTMAP},  // Output map file
    {"-help", no_argument, OPT_HELP},
    {"v", no_argument, OPT_VERSION},
    {"-version", no_argument, OPT_VERSION},

    {"no-pie", no_argument, OPT_NO_PIE},
    {NULL},
  };

  Vector *sources = new_vector();
  Vector *lib_paths = new_vector();
  int error_count = 0;
  for (;;) {
    int opt = optparse(argc, argv, kOptions);
    if (opt == -1) {
      if (optind >= argc)
        break;

      vec_push(sources, argv[optind++]);
      continue;
    }

    switch (opt) {
    default: assert(false); break;
    case OPT_HELP:
      usage(stdout);
      exit(0);
    case OPT_VERSION:
      show_version("ld", XCC_TARGET_ARCH);
      break;
    case 'o':
      opts->ofn = optarg;
      break;
    case 'e':
      opts->entry = optarg;
      break;
    case 'l':
      {
        const char *path = search_library(lib_paths, optarg);
        if (path != NULL) {
          vec_push(sources, path);
        } else {
          fprintf(stderr, "%s: library not found\n", optarg);
          ++error_count;
        }
      }
      break;
    case 'L':
      vec_push(lib_paths, optarg);
      break;
    case OPT_OUTMAP:
      opts->outmapfn = optarg;
      break;
    case OPT_NO_PIE:
      // Silently ignored.
      break;
    case '?':
      fprintf(stderr, "Warning: unknown option: %s\n", argv[optind - 1]);
      break;
    }
  }
  if (error_count > 0)
    exit(1);

  return sources;
}

typedef struct {
  enum LinkElemKind kind;
  union {
    struct {
      const char *name;
    } section;
    struct {
      const char *name;
    } symbol;
    size_t align;
  };
} ElemData;

static const ElemData kCodeSectionNames[] = {
  {.kind = LEK_SECTION, .section = {.name = ".text"}},
  {.kind = LEK_SECTION, .section = {.name = ".rodata"}},
  {.kind = -1},
};
static const ElemData kDataSectionNames[] = {
  {.kind = LEK_SECTION, .section = {.name = ".data"}},

  {.kind = LEK_ALIGN, .align = 8},
  {.kind = LEK_SYMBOL, .symbol = {.name = "__init_array_start"}},
  {.kind = LEK_SECTION, .section = {.name = ".init_array"}},
  {.kind = LEK_SYMBOL, .symbol = {.name = "__init_array_end"}},

  {.kind = LEK_SYMBOL, .symbol = {.name = "__fini_array_start"}},
  {.kind = LEK_SECTION, .section = {.name = ".fini_array"}},
  {.kind = LEK_SYMBOL, .symbol = {.name = "__fini_array_end"}},

  {.kind = LEK_SECTION, .section = {.name = ".bss"}},
  {.kind = -1},
};
static const ElemData *kSectionNames[] = {
  [SEC_TEXT] = kCodeSectionNames,
  [SEC_DATA] = kDataSectionNames,
};

static void prepare_section_lists(LinkEditor *ld, Vector *section_lists[SECTION_COUNT]) {
  for (int secno = 0; secno < SECTION_COUNT; ++secno) {
    Vector *seclist = new_vector();
    section_lists[secno] = seclist;
    for (const ElemData *p = kSectionNames[secno]; (int)p->kind >= 0; ++p) {
      LinkElem *elem = NULL;
      switch (p->kind) {
      case LEK_SECTION:
        elem = new_link_elem(LEK_SECTION);
        elem->section.name = p->section.name;
        elem->section.list = new_vector();
        break;
      case LEK_SYMBOL:
        elem = new_link_elem(LEK_SYMBOL);
        elem->symbol.address = 0;
        table_put(ld->generated_symbol_table, alloc_name(p->symbol.name, NULL, false), elem);
        break;
      case LEK_ALIGN:
        elem = new_link_elem(LEK_ALIGN);
        elem->align = p->align;
        break;
      }
      if (elem != NULL)
        vec_push(seclist, elem);
    }
  }
}

static void collect_sections(LinkEditor *ld, Vector *section_lists[SECTION_COUNT]) {
  for (int secno = 0; secno < SECTION_COUNT; ++secno) {
    Vector *seclist = section_lists[secno];
    for (int i = 0; i < seclist->len; ++i) {
      LinkElem *elem = seclist->data[i];
      switch (elem->kind) {
      case LEK_SECTION:
        ld_collect_sections(ld, elem->section.name, elem->section.list);
        break;
      case LEK_SYMBOL:
      case LEK_ALIGN:
        break;
      }
    }
  }
}

static void prepare_section_groups(Vector *section_lists[SECTION_COUNT],
                                   SectionGroup section_groups[SECTION_COUNT]) {
  for (int secno = 0; secno < SECTION_COUNT; ++secno) {
    SectionGroup *secgroup = &section_groups[secno];
    secgroup->align = secno == SEC_DATA ? DATA_ALIGN : 1;
    secgroup->start_address = 0;
    secgroup->ds = NULL;
    secgroup->bss_size = 0;
    secgroup->ds = calloc_or_die(sizeof(*secgroup->ds));
    data_init(secgroup->ds);

    Vector *seclist = section_lists[secno];
    for (int i = 0; i < seclist->len; ++i) {
      LinkElem *elem = seclist->data[i];
      switch (elem->kind) {
      case LEK_SECTION:
        {
          Vector *list = elem->section.list;
          for (int j = 0; j < list->len; ++j) {
            ElfSectionInfo *p = list->data[j];
            Elf64_Xword align = p->shdr->sh_addralign;
            if (align > secgroup->align)
              secgroup->align = align;
          }
        }
        break;
      case LEK_SYMBOL:
        break;
      case LEK_ALIGN:
        {
          Elf64_Xword align = elem->align;
          if (align > secgroup->align)
            secgroup->align = align;
        }
        break;
      }
    }
  }
}

static void collect_section_data(Vector *section_lists[SECTION_COUNT],
                                 SectionGroup section_groups[SECTION_COUNT]) {
  for (int secno = 0; secno < SECTION_COUNT; ++secno) {
    Vector *v = section_lists[secno];
    for (int i = 0; i < v->len; ++i) {
      LinkElem *elem = v->data[i];
      switch (elem->kind) {
      case LEK_SECTION:
        {
          Vector *list = elem->section.list;
          for (int j = 0; j < list->len; ++j) {
            ElfSectionInfo *p = list->data[j];
            add_elf_section(section_groups, p, secno);
          }
        }
        break;
      case LEK_SYMBOL:
        break;
      case LEK_ALIGN:
        {
          SectionGroup *secgroup = &section_groups[secno];
          data_align(secgroup->ds, elem->align);
        }
        break;
      }
    }
  }
}

static int do_link(Vector *sources, const Options *opts) {
  LinkEditor *ld = malloc_or_die(sizeof(*ld));
  ld_init(ld, sources->len);
  for (int i = 0; i < sources->len; ++i) {
    char *src = sources->data[i];
    ld_load(ld, i, src);
  }

  const Name *entry_name = alloc_name(opts->entry, NULL, false);
  Table unresolved;
  table_init(&unresolved);
  table_put(&unresolved, entry_name, (void*)entry_name);

  Vector *section_lists[SECTION_COUNT];  // <LinkElem*>
  prepare_section_lists(ld, section_lists);

  if (ld_resolve_symbols(ld, &unresolved) > 0)
    return 1;
  if (unresolved.count > 0) {
    fprintf(stderr, "Unresolved: #%d\n", unresolved.count);
    const Name *name;
    for (int it = 0; (it = table_iterate(&unresolved, it, &name, NULL)) != -1;) {
      fprintf(stderr, "  %.*s\n", NAMES(name));
    }
    return 1;
  }

  collect_sections(ld, section_lists);

  SectionGroup section_groups[SECTION_COUNT];
  prepare_section_groups(section_lists, section_groups);

  ld_calc_address(section_groups, section_lists, LOAD_ADDRESS);
  ld_load_elf_objects(section_lists);

  int error_count = ld_resolve_relas(ld);
  if (error_count > 0)
    return 1;

  collect_section_data(section_lists, section_groups);

  uint64_t entry_address = ld_symbol_address(ld, entry_name);
  assert(entry_address != (uint64_t)-1);

  bool result = output_exe(opts->ofn, entry_address, section_groups);

  if (opts->outmapfn != NULL && result)
    result = output_map_file(ld, opts->outmapfn, entry_address, entry_name);
  return result ? 0 : 1;
}

int main(int argc, char *argv[]) {
  Options opts = {
    .ofn = NULL,
    .entry = kDefaultEntryName,
    .outmapfn = NULL,
  };
  Vector *sources = parse_options(argc, argv, &opts);

  if (sources->len == 0)
    error("no input");

  if (opts.ofn == NULL)
    opts.ofn = "a.out";

  return do_link(sources, &opts);
}
