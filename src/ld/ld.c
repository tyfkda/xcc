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

typedef struct {
  size_t align;
  uintptr_t start_address;
  DataStorage *ds;
  size_t bss_size;
} SectionGroup;

typedef struct {
  File *files;
  int nfiles;
  Table *symbol_table;  // <ElfObj*>
} LinkEditor;

void ld_init(LinkEditor *ld, int nfiles) {
  ld->files = calloc_or_die(sizeof(*ld->files) * nfiles);
  ld->nfiles = nfiles;
  ld->symbol_table = alloc_table();
  assert(ld->symbol_table != NULL);
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

static uintptr_t ld_symbol_address(LinkEditor *ld, const Name *name) {
  ElfObj *elfobj = table_get(ld->symbol_table, name);
  if (elfobj != NULL) {
    Elf64_Sym *sym = elfobj_find_symbol(elfobj, name);
    assert(sym != NULL && sym->st_shndx != SHN_UNDEF);
    if (sym->st_shndx < SHN_LORESERVE) {
#ifndef NDEBUG
      const Elf64_Shdr *tshdr = &elfobj->shdrs[sym->st_shndx];
      assert(tshdr->sh_type == SHT_PROGBITS || tshdr->sh_type == SHT_NOBITS);
#endif
      return elfobj->section_infos[sym->st_shndx].progbits.address + sym->st_value;
    }

    switch (sym->st_shndx) {
    case SHN_COMMON:
      assert(!"Unhandled shndx:common");
      break;
    default:
      assert(!"Unhandled shndx");
      break;
    }
  }
  return (uintptr_t)-1;
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
#define ITYPE(imm, rs1, funct3, rd, opcode)          MAKE_CODE32((IMM(imm, 11, 0) << 20) | ((rs1) << 15) | ((funct3) << 12) | ((rd) << 7) | (opcode))
#define UTYPE(imm, rd, opcode)                       MAKE_CODE32((IMM(imm, 31, 12) << 12) | ((rd) << 7) | (opcode))
#define W_JAL(rd, imm)            UTYPE(SWIZZLE_JAL(imm), rd, 0x6f)
#define W_ADDI(rd, rs, imm)       ITYPE(imm, rs, 0x00, rd, 0x13)
#define P_NOP()                   W_ADDI(ZERO, ZERO, 0)

static uintptr_t calc_rela_sym_address(LinkEditor *ld, ElfObj *elfobj, const Elf64_Rela *rela, const Elf64_Sym *sym, const ElfSectionInfo *strinfo) {
  uintptr_t address = 0;
  switch (ELF64_ST_BIND(sym->st_info)) {
  case STB_LOCAL:
    if (ELF64_ST_TYPE(sym->st_info) == STT_SECTION) {
      assert(ELF64_R_SYM(rela->r_info) < elfobj->ehdr.e_shnum);
      const ElfSectionInfo *s = &elfobj->section_infos[ELF64_R_SYM(rela->r_info)];
      address = s->progbits.address;
    } else {
      uintptr_t sectop = elfobj->section_infos[sym->st_shndx].progbits.address;
      address = sectop + sym->st_value;
    }
    break;
  case STB_GLOBAL:
    {
      const char *label = &strinfo->strtab.buf[sym->st_name];
      address = ld_symbol_address(ld, alloc_name(label, NULL, false));
      assert(address != (uintptr_t)-1);
    }
    break;
  default: assert(false); break;
  }

  return address + rela->r_addend;
}

static int resolve_rela_elfobj(LinkEditor *ld, ElfObj *elfobj) {
  int error_count = 0;
  for (Elf64_Half sec = 0; sec < elfobj->ehdr.e_shnum; ++sec) {
    Elf64_Shdr *shdr = &elfobj->shdrs[sec];
    if (shdr->sh_type != SHT_RELA || shdr->sh_size <= 0)
      continue;
    const Elf64_Rela *relas = read_or_die(elfobj->fp, NULL, shdr->sh_offset + elfobj->start_offset,
                                          shdr->sh_size, "read error");
    const Elf64_Shdr *symhdr = &elfobj->shdrs[shdr->sh_link];
    const ElfSectionInfo *symhdrinfo = &elfobj->section_infos[shdr->sh_link];
    const ElfSectionInfo *strinfo = &elfobj->section_infos[symhdr->sh_link];
    const ElfSectionInfo *dst_info = &elfobj->section_infos[shdr->sh_info];
    assert(dst_info->shdr->sh_type == SHT_PROGBITS);
    if (dst_info->progbits.content == NULL) {
      // Target section is not collected, so skip relocation.
      continue;
    }

    size_t symbol_count = elfobj->symtab_section->shdr->sh_size / sizeof(Elf64_Sym);
    for (size_t j = 0, n = shdr->sh_size / sizeof(Elf64_Rela); j < n; ++j) {
      const Elf64_Rela *rela = &relas[j];
      assert(ELF64_R_SYM(rela->r_info) < symbol_count);
      const Elf64_Sym *sym = &symhdrinfo->symtab.syms[ELF64_R_SYM(rela->r_info)];
      uintptr_t address = calc_rela_sym_address(ld, elfobj, rela, sym, strinfo);

      void *p = dst_info->progbits.content + rela->r_offset;
      uintptr_t pc = elfobj->section_infos[shdr->sh_info].progbits.address + rela->r_offset;
      switch (ELF64_R_TYPE(rela->r_info)) {
#if XCC_TARGET_ARCH == XCC_ARCH_X64
      case R_X86_64_64:
        *(uint64_t*)p = address;
        break;
      case R_X86_64_PC32:
      case R_X86_64_PLT32:
        *(uint32_t*)p = address - pc;
        break;

#elif XCC_TARGET_ARCH == XCC_ARCH_AARCH64
      case R_AARCH64_ABS64:
        *(uint64_t*)p = address;
        break;
      case R_AARCH64_ADR_PREL_PG_HI21:  // Page(S+A)-Page(P)
        {
          const int PAGE = 12;
          const uint32_t MASK = ~0x60ffffe0;
          uint32_t d = (address >> PAGE) - (pc >> PAGE);
          *(uint32_t*)p = (*(uint32_t*)p & MASK) | ((d & 0x03) << 29) | ((d & 0x1ffffc) << 3);
        }
        break;
      case R_AARCH64_ADD_ABS_LO12_NC:  // S + A
        {
          const uint32_t MASK = ~(((1U << 12) - 1) << 10);
          *(uint32_t*)p = (*(uint32_t*)p & MASK) | ((address << 10) & ~MASK);
        }
        break;
      case R_AARCH64_CALL26:  // S+A-P
        {
          const uint32_t MASK = -(1U << 26);
          *(uint32_t*)p = (*(uint32_t*)p & MASK) | (((address - pc) >> 2) & ~MASK);
        }
        break;

      case R_AARCH64_ADR_GOT_PAGE:
      case R_AARCH64_LD64_GOT_LO12_NC:
        assert(!"TODO: Implement");
        break;

#elif XCC_TARGET_ARCH == XCC_ARCH_RISCV64
      case R_RISCV_64:
        *(uint64_t*)p = address;
        break;
      case R_RISCV_CALL:
        {
          int64_t offset = address - pc;
          assert(offset < (1L << 19) && offset >= -(1L << 19));  // TODO
          *(uint32_t*)p = W_JAL(RA, offset);
        }
        break;
      case R_RISCV_RELAX:
        {
          // TODO: Check
          assert(j > 0);
          const Elf64_Rela *rela0 = &relas[j - 1];
          switch (ELF64_R_TYPE(rela0->r_info)) {
          case R_RISCV_CALL:
            ((uint32_t*)p)[1] = P_NOP();
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
          int64_t offset = address - (ELF64_R_TYPE(rela->r_info) == R_RISCV_PCREL_HI20 ? pc : 0);
          assert(offset < (1L << 31) && offset >= -(1L << 31));
          // const uint32_t MASK20 = (1U << 20) - 1;
          const uint32_t MASK12 = (1U << 12) - 1;
          if ((offset & MASK12) >= (1U << 11))
            offset += 1U << 12;
          *(uint32_t*)p = (*(uint32_t*)p & MASK12) | ((uint32_t)offset & ~MASK12);
        }
        break;
      case R_RISCV_PCREL_LO12_I:
      case R_RISCV_LO12_I:
        {
          // Get corresponding HI20 rela, and calculate the offset.
          // Assume [..., [j-2]=PCREL_HI20, [j-1]=RELAX, [j]=PCREL_LO12_I, ...]
          assert(j >= 2);
          const Elf64_Rela *hirela = &relas[j - 2];
          assert((ELF64_R_TYPE(rela->r_info) == R_RISCV_PCREL_LO12_I && ELF64_R_TYPE(hirela->r_info) == R_RISCV_PCREL_HI20) ||
                 (ELF64_R_TYPE(rela->r_info) == R_RISCV_LO12_I && ELF64_R_TYPE(hirela->r_info) == R_RISCV_HI20));
          const Elf64_Sym *hisym = &symhdrinfo->symtab.syms[ELF64_R_SYM(hirela->r_info)];
          uintptr_t hiaddress = calc_rela_sym_address(ld, elfobj, hirela, hisym, strinfo);
          uintptr_t hipc = elfobj->section_infos[shdr->sh_info].progbits.address + hirela->r_offset;

          int64_t offset = hiaddress - (ELF64_R_TYPE(rela->r_info) == R_RISCV_PCREL_LO12_I ? hipc : 0);
          assert(offset < (1L << 31) && offset >= -(1L << 31));
          const uint32_t MASK20 = (1U << 20) - 1;
          const uint32_t MASK12 = (1U << 12) - 1;
          *(uint32_t*)p = (*(uint32_t*)p & MASK20) | (((uint32_t)offset & MASK12) << 20);
        }
        break;
      case R_RISCV_RVC_JUMP:
        {
          int64_t offset = address - pc;
          assert(offset < (1L << 11) && offset >= -(1L << 11));

          uint16_t *q = (uint16_t*)p;
          assert((*q & 0xe003) == 0xa001);  // c.j
          *q = (*q & 0xe003) | SWIZZLE_C_J(offset);
        }
        break;
      case R_RISCV_JAL:
        {
          int64_t offset = address - pc;
          assert(offset < (1L << 19) && offset >= -(1L << 19));

          uint32_t *q = (uint32_t*)p;
          assert((*q & 0x0000007f) == 0x6f);  // jal
          *q = (*q & 0x0000007f) | SWIZZLE_JAL(offset);
        }
        break;
      case R_RISCV_BRANCH:
      case R_RISCV_RVC_BRANCH:
        {
          int64_t offset = address - pc;

          if (ELF64_R_TYPE(rela->r_info) == R_RISCV_RVC_BRANCH) {
            assert(offset < (1 << 7) && offset >= -(1 << 7));
            // c.beqz, c.bnez
            uint16_t *q = (uint16_t*)p;
            assert((*q & 0xc003) == 0xc001);
            *q = (*q & 0xe383) | SWIZZLE_C_BXX(offset);
          } else {
            assert(offset < (1 << 11) && offset >= -(1 << 11));
            uint32_t *q = (uint32_t*)p;
            *q = (*q & 0x01fff07f) | SWIZZLE_BXX(offset);
          }
        }
        break;
#endif

      default:
        fprintf(stderr, "Unhandled rela type: %" PRIx32 "\n", (uint32_t)ELF64_R_TYPE(rela->r_info));
        ++error_count;
        break;
      }
    }
  }
  return error_count;
}

static void resolve_symbols_elfobj(LinkEditor *ld, ElfObj *elfobj, Table *unresolved) {
  ElfSectionInfo *symtab_section = elfobj->symtab_section;
  assert(symtab_section != NULL);
  ElfSectionInfo *strtab_section = symtab_section->symtab.strtab;
  const char *str = strtab_section->strtab.buf;

  Table *defined = ld->symbol_table;
  Table *symbol_table = elfobj->symbol_table;
  const Name *name;
  Elf64_Sym *sym;
  for (int it = 0; (it = table_iterate(symbol_table, it, &name, (void**)&sym)) != -1; ) {
    if (ELF64_ST_TYPE(sym->st_info) == STT_SECTION || str[sym->st_name] == '\0')
      continue;
    const Name *name = alloc_name(&str[sym->st_name], NULL, false);

    if (table_try_get(defined, name, NULL)) {
      if (sym->st_shndx != SHN_UNDEF) {
        // TODO: Raise duplicate symbol error.
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
}

static void collect_sections_elfobj(LinkEditor *ld, ElfObj *elfobj, const char *name, Vector *seclist) {
  UNUSED(ld);
  ElfSectionInfo *shsymtab = &elfobj->section_infos[elfobj->ehdr.e_shstrndx];
  assert(shsymtab->shdr->sh_type == SHT_STRTAB);
  const char *strbuf = shsymtab->strtab.buf;
  for (int i = 0; i < elfobj->prog_sections->len; ++i) {
    ElfSectionInfo *section = elfobj->prog_sections->data[i];
    if (section == NULL)
      continue;
    Elf64_Shdr *shdr = section->shdr;
    assert(shdr->sh_type == SHT_PROGBITS || shdr->sh_type == SHT_NOBITS);
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

static void resolve_symbols_archive(LinkEditor *ld, Archive *ar, Table *unresolved) {
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
        resolve_symbols_elfobj(ld, elfobj, unresolved);
        retry = true;
        break;
      }
    }
    if (!retry)
      break;
  }
}

static void collect_sections_archive(LinkEditor *ld, Archive *ar, const char *name, Vector *seclist) {
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

static void add_elf_section(SectionGroup section_groups[SECTION_COUNT], const ElfSectionInfo *p, int secno) {
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

static void ld_resolve_symbols(LinkEditor *ld, Table *unresolved) {
  for (int i = 0; i < ld->nfiles; ++i) {
    File *file = &ld->files[i];
    switch (file->kind) {
    case FK_ELFOBJ:
      resolve_symbols_elfobj(ld, file->elfobj, unresolved);
      break;
    case FK_ARCHIVE:
      resolve_symbols_archive(ld, file->archive, unresolved);
      break;
    }
  }
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

static void ld_calc_address(SectionGroup section_groups[SECTION_COUNT], Vector *section_lists[SECTION_COUNT], uintptr_t start_address) {
  uintptr_t address = start_address;
  for (int secno = 0; secno < SECTION_COUNT; ++secno) {
    Vector *v = section_lists[secno];
    if (v->len <= 0)
      continue;

    SectionGroup *secgroup = &section_groups[secno];
    secgroup->start_address = address = ALIGN(address, secgroup->align);
    for (int i = 0; i < v->len; ++i) {
      ElfSectionInfo *p = v->data[i];
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
}

static void ld_load_elf_objects(Vector *section_lists[SECTION_COUNT]) {
  for (int secno = 0; secno < SECTION_COUNT; ++secno) {
    Vector *v = section_lists[secno];
    if (v->len <= 0)
      continue;

    for (int i = 0; i < v->len; ++i) {
      ElfSectionInfo *p = v->data[i];
      Elf64_Shdr *shdr = p->shdr;
      if (shdr->sh_type == SHT_PROGBITS) {
        Elf64_Xword size = shdr->sh_size;
        assert(size > 0);

        ElfObj *elfobj = p->elfobj;
        void *buf = read_or_die(elfobj->fp, NULL, shdr->sh_offset + elfobj->start_offset, size, "read error");
        p->progbits.content = buf;
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

static bool output_exe(const char *ofn, uintptr_t entry_address, SectionGroup section_groups[SECTION_COUNT]) {
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
  out_program_header(fp, 0, PROG_START, section_groups[SEC_TEXT].start_address, code_rodata_sz, code_rodata_sz);
  if (phnum > 1) {
    size_t datamemsz = section_groups[SEC_DATA].ds->len + section_groups[SEC_DATA].bss_size;
    uintptr_t offset = PROG_START + code_rodata_sz;
    if (section_groups[SEC_DATA].ds->len > 0)
      offset = ALIGN(offset, DATA_ALIGN);
    out_program_header(fp, 1, offset, section_groups[SEC_DATA].start_address, section_groups[SEC_DATA].ds->len, datamemsz);
  }

  uintptr_t addr = PROG_START;
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

static void dump_map_elfobj(LinkEditor *ld, ElfObj *elfobj, File *file, ArContent *content, FILE *fp) {
  const Name *name;
  Elf64_Sym* sym;
  for (int it = 0; (it = table_iterate(elfobj->symbol_table, it, &name, (void**)&sym)) != -1; ) {
    if (sym->st_shndx == SHN_UNDEF)
      continue;

    uintptr_t address = 0;
    switch (ELF64_ST_BIND(sym->st_info)) {
    case STB_LOCAL:
      {
        const ElfSectionInfo *s = &elfobj->section_infos[sym->st_shndx];
        address = s->progbits.address;
      }
      break;
    case STB_GLOBAL:
      address = ld_symbol_address(ld, name);
      break;
    default: assert(false); break;
    }
    fprintf(fp, "%9lx: %.*s  (%s", address, NAMES(name), file->filename);
    if (content != NULL)
      fprintf(fp, ", %s", content->name);
    fprintf(fp, ")\n");
  }
}

static void dump_map_file(LinkEditor *ld, FILE *fp) {
  for (int i = 0; i < ld->nfiles; ++i) {
    File *file = &ld->files[i];
    switch (file->kind) {
    case FK_ELFOBJ:
      dump_map_elfobj(ld, file->elfobj, file, NULL, fp);
      break;
    case FK_ARCHIVE:
      {
        Archive *ar = file->archive;
        FOREACH_FILE_ARCONTENT(ar, content, {
          dump_map_elfobj(ld, content->obj, file, content, fp);
        });
      }
      break;
    }
  }
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
    {"-version", no_argument, 'V'},

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
    case 'V':
      show_version("ld");
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
    default:
      fprintf(stderr, "Warning: unknown option: %s\n", argv[optind - 1]);
      break;
    }
  }
  if (error_count > 0)
    exit(1);

  return sources;
}

static const char *kCodeSectionNames[] = {".text", ".rodata", NULL};
static const char *kDataSectionNames[] = {".data", ".bss", NULL};
static const char **kSectionNames[] = {
  [SEC_TEXT] = kCodeSectionNames,
  [SEC_DATA] = kDataSectionNames,
};

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

  ld_resolve_symbols(ld, &unresolved);
  if (unresolved.count > 0) {
    fprintf(stderr, "Unresolved: #%d\n", unresolved.count);
    const Name *name;
    for (int it = 0; (it = table_iterate(&unresolved, it, &name, NULL)) != -1;) {
      fprintf(stderr, "  %.*s\n", NAMES(name));
    }
    return 1;
  }

  Vector *section_lists[SECTION_COUNT];  // <ElfSectionInfo*>
  for (int secno = 0; secno < SECTION_COUNT; ++secno) {
    Vector *seclist = new_vector();
    for (const char **pp = kSectionNames[secno]; *pp != NULL; ++pp) {
      ld_collect_sections(ld, *pp, seclist);
      section_lists[secno] = seclist;
    }
  }

  SectionGroup section_groups[SECTION_COUNT];
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
      ElfSectionInfo *p = seclist->data[i];
      Elf64_Xword align = p->shdr->sh_addralign;
      if (align > secgroup->align)
        secgroup->align = align;
    }
  }
  ld_calc_address(section_groups, section_lists, LOAD_ADDRESS);
  ld_load_elf_objects(section_lists);

  int error_count = ld_resolve_relas(ld);
  if (error_count > 0)
    return 1;

  for (int secno = 0; secno < SECTION_COUNT; ++secno) {
    Vector *v = section_lists[secno];
    for (int i = 0; i < v->len; ++i) {
      ElfSectionInfo *p = v->data[i];
      add_elf_section(section_groups, p, secno);
    }
  }

  uintptr_t entry_address = ld_symbol_address(ld, entry_name);
  assert(entry_address != (uintptr_t)-1);

  bool result = output_exe(opts->ofn, entry_address, section_groups);

  if (opts->outmapfn != NULL && result) {
    FILE *mapfp;
    if (strcmp(opts->outmapfn, "-") == 0) {
      mapfp = stdout;
    } else {
      mapfp = fopen(opts->outmapfn, "w");
      if (mapfp == NULL)
        perror("Failed to open map file");
    }

    fprintf(mapfp, "### Symbols\n");
    fprintf(mapfp, "%9lx:  (start address)\n", (long)LOAD_ADDRESS);
    dump_map_file(ld, mapfp);

    fprintf(mapfp, "\n### Entry point\n");
    fprintf(mapfp, "%9lx: %.*s\n", entry_address, NAMES(entry_name));

    if (mapfp != stdout)
      fclose(mapfp);
  }
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
