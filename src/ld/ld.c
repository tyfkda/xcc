#include <assert.h>
#include <getopt.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "elfutil.h"
#include "gen_section.h"
#include "table.h"
#include "util.h"

static const char kDefaultEntryName[] = "_start";

#define PROG_START   (0x100)

#if defined(__XV6)
// XV6
#include "../kernel/syscall.h"
#include "../kernel/traps.h"

#define START_ADDRESS    0x1000

#else
// *nix

#if defined(__linux__)
#include <elf.h>
#else
#include "../../include/elf.h"
#endif

#include <sys/stat.h>

#define START_ADDRESS    (0x01000000 + PROG_START)

#endif

#define LOAD_ADDRESS    START_ADDRESS
#define DATA_ALIGN      (0x1000)

void *malloc_or_die(size_t size) {
  void *p = malloc(size);
  if (p != NULL)
    return p;
  fprintf(stderr, "memory overflow\n");
  exit(1);
}

void* read_from(FILE *fp, unsigned long offset, size_t size) {
  void *buf = malloc_or_die(size);
  if (fseek(fp, offset, SEEK_SET) != 0 ||
      fread(buf, 1, size, fp) != size) {
    free(buf);
    buf = NULL;
  }
  return buf;
}

static void put_padding(FILE *fp, uintptr_t start) {
  long cur = ftell(fp);
  if (start > (size_t)cur) {
    size_t size = start - (uintptr_t)cur;
    char *buf = calloc(1, size);
    fwrite(buf, size, 1, fp);
    free(buf);
  }
}

static Elf64_Shdr *read_all_section_headers(FILE *fp, Elf64_Ehdr *ehdr) {
  if (ehdr->e_shnum <= 0)
    return NULL;
  Elf64_Shdr *esecs = read_from(fp, ehdr->e_shoff, ehdr->e_shnum * sizeof(Elf64_Shdr));
  if (esecs == NULL) {
    perror("read section header failed");
  }
  return esecs;
}

static char *read_strtab(FILE *fp, Elf64_Shdr *sec) {
  assert(sec->sh_type == SHT_STRTAB);
  char *buf = read_from(fp, sec->sh_offset, sec->sh_size);
  if (buf == NULL) {
    perror("read strtab failed");
  }
  return buf;
}

typedef struct {
  struct ElfObj *elfobj;
  Elf64_Shdr *shdr;
  union {
    struct {
      uintptr_t address;
      unsigned char *content;
    } progbits;
    struct {
      const char *buf;
    } strtab;
    struct {
      Elf64_Sym *symtabs;
      Table names;  // <Elf64_Sym*>
    } symtab;
  };
} SectionInfo;

typedef struct ElfObj {
  FILE *fp;
  Elf64_Ehdr ehdr;
  Elf64_Shdr *shdrs;
  char *shstrtab;

  SectionInfo *section_infos;
} ElfObj;

static void elfobj_init(ElfObj *elfobj) {
  memset(elfobj, 0, sizeof(*elfobj));
}

static bool load_symtab(ElfObj *elfobj) {
  for (Elf64_Half sec = 0; sec < elfobj->ehdr.e_shnum; ++sec) {
    Elf64_Shdr *shdr = &elfobj->shdrs[sec];
    switch (shdr->sh_type) {
    case SHT_STRTAB:
      {
        const char *buf = read_strtab(elfobj->fp, shdr);
        if (buf == NULL)
          error("read strtab failed");
        elfobj->section_infos[sec].strtab.buf = buf;
      }
      break;
    case SHT_SYMTAB:
      {
        Elf64_Sym *symtabs = read_from(elfobj->fp, shdr->sh_offset, shdr->sh_size);
        if (symtabs == NULL)
          perror("read symtab failed");
        if (shdr->sh_size % sizeof(Elf64_Sym) != 0)
          error("malformed symtab");

        Elf64_Shdr *strtab_sec = &elfobj->shdrs[shdr->sh_link];
        if (strtab_sec->sh_type != SHT_STRTAB)
          error("malformed symtab");

        SectionInfo *p = &elfobj->section_infos[sec];
        table_init(&p->symtab.names);
        p->symtab.symtabs = symtabs;
      }
      break;
    }
  }

  for (Elf64_Half sec = 0; sec < elfobj->ehdr.e_shnum; ++sec) {
    Elf64_Shdr *shdr = &elfobj->shdrs[sec];
    if (shdr->sh_type != SHT_SYMTAB)
      continue;

    SectionInfo *p = &elfobj->section_infos[sec];
    SectionInfo *q = &elfobj->section_infos[shdr->sh_link];  // Strtab
    const char *str = q->strtab.buf;
    Table *names = &p->symtab.names;
    size_t count = shdr->sh_size / sizeof(Elf64_Sym);
    for (uint32_t i = 0; i < count; ++i) {
      Elf64_Sym *sym = &p->symtab.symtabs[i];
      unsigned char type = sym->st_info & 0x0f;
      if (type == STT_NOTYPE && str[sym->st_name] != '\0') {
        const Name *name = alloc_name(&str[sym->st_name], NULL, false);
        table_put(names, name, sym);
      }
    }
  }

  return true;
}

static bool read_elf(ElfObj *elfobj, FILE *fp, const char *fn) {
  elfobj->fp = fp;
  ssize_t size = fread(&elfobj->ehdr, sizeof(elfobj->ehdr), 1, fp);
  if (size != 1 ||
      elfobj->ehdr.e_ident[0] != ELFMAG0 || elfobj->ehdr.e_ident[1] != ELFMAG1 ||
      elfobj->ehdr.e_ident[2] != ELFMAG2 || elfobj->ehdr.e_ident[3] != ELFMAG3) {
    fprintf(stderr, "no elf file: %s\n", fn);
    return false;
  }
  if (elfobj->ehdr.e_machine != EM_X86_64 || elfobj->ehdr.e_version != EV_CURRENT ||
      elfobj->ehdr.e_ehsize != sizeof(Elf64_Ehdr) || elfobj->ehdr.e_shentsize != sizeof(Elf64_Shdr) ||
      elfobj->ehdr.e_shnum < 1 || elfobj->ehdr.e_shstrndx >= elfobj->ehdr.e_shnum) {
    fprintf(stderr, "illegal elf: %s\n", fn);
    return false;
  }
  elfobj->shdrs = read_all_section_headers(fp, &elfobj->ehdr);
  if (elfobj->shdrs != NULL) {
    SectionInfo *section_infos = calloc(elfobj->ehdr.e_shnum, sizeof(*elfobj->section_infos));
    elfobj->section_infos = section_infos;
    for (unsigned short i = 0; i < elfobj->ehdr.e_shnum; ++i) {
      Elf64_Shdr *shdr = &elfobj->shdrs[i];
      SectionInfo *p = &section_infos[i];
      p->elfobj = elfobj;
      p->shdr = shdr;
    }

    elfobj->shstrtab = read_strtab(fp, &elfobj->shdrs[elfobj->ehdr.e_shstrndx]);
    if (elfobj->shstrtab != NULL) {
      if (!load_symtab(elfobj))
        return false;
    }
  }
  return true;
}

static bool open_elf(const char *fn, ElfObj *elfobj) {
  FILE *fp = fopen(fn, "r");
  if (fp == NULL) {
    fprintf(stderr, "cannot open: %s\n", fn);
  } else {
    if (read_elf(elfobj, fp, fn))
      return true;
    fclose(fp);
    elfobj->fp = NULL;
  }
  return false;
}

static void close_elf(ElfObj *elfobj) {
  if (elfobj->fp != NULL) {
    fclose(elfobj->fp);
    elfobj->fp = NULL;
  }
}

static Elf64_Sym *find_symbol(ElfObj *elfobj, const Name *name) {
  for (Elf64_Half sec = 0; sec < elfobj->ehdr.e_shnum; ++sec) {
    Elf64_Shdr *shdr = &elfobj->shdrs[sec];
    if (shdr->sh_type != SHT_SYMTAB)
      continue;
    SectionInfo *p = &elfobj->section_infos[sec];
    Elf64_Sym *sym = table_get(&p->symtab.names, name);
    if (sym != NULL)
      return sym;
  }
  return NULL;
}

static Elf64_Sym *find_symbol_from_all(Vector *elfobjs, const Name *name, ElfObj **pelfobj) {
  for (int i = 0; i < elfobjs->len; ++i) {
    ElfObj *elfobj = elfobjs->data[i];
    Elf64_Sym *sym = find_symbol(elfobj, name);
    if (sym != NULL && sym->st_shndx != 0) {
      if (pelfobj != NULL)
        *pelfobj = elfobj;
      return sym;
    }
  }
  return NULL;
}

static void resolve_relas(Vector *elfobjs) {
  for (int i = 0; i < elfobjs->len; ++i) {
    ElfObj *elfobj = elfobjs->data[i];
    for (Elf64_Half sec = 0; sec < elfobj->ehdr.e_shnum; ++sec) {
      Elf64_Shdr *shdr = &elfobj->shdrs[sec];
      if (shdr->sh_type != SHT_RELA || shdr->sh_size <= 0)
        continue;
      const Elf64_Rela *relas = read_from(elfobj->fp, shdr->sh_offset, shdr->sh_size);
      if (relas == NULL) {
        perror("read error");
      }
      const Elf64_Shdr *symhdr = &elfobj->shdrs[shdr->sh_link];
      const SectionInfo *symhdrinfo = &elfobj->section_infos[shdr->sh_link];
      const SectionInfo *strinfo = &elfobj->section_infos[symhdr->sh_link];
      assert(elfobj->shdrs[shdr->sh_info].sh_type == SHT_PROGBITS);
      const SectionInfo *dst_info = &elfobj->section_infos[shdr->sh_info];
      for (size_t j = 0, n = shdr->sh_size / sizeof(Elf64_Rela); j < n; ++j) {
        const Elf64_Rela *rela = &relas[j];
        unsigned char type = ELF64_R_TYPE(rela->r_info);
        switch (type) {
        case R_X86_64_64:
          {
            const Elf64_Sym *sym = &symhdrinfo->symtab.symtabs[ELF64_R_SYM(rela->r_info)];
            switch (ELF64_ST_BIND(sym->st_info)) {
            case STB_LOCAL:
              {
                assert(ELF64_R_SYM(rela->r_info) < elfobj->ehdr.e_shnum);
                const Elf64_Shdr *tshdr = &elfobj->shdrs[ELF64_R_SYM(rela->r_info)];
                assert(tshdr->sh_type == SHT_PROGBITS || tshdr->sh_type == SHT_NOBITS);

                const SectionInfo *s = &elfobj->section_infos[ELF64_R_SYM(rela->r_info)];
                intptr_t offset = s->progbits.address;
                // intptr_t current = elfobj->section_infos[shdr->sh_info].progbits.address;
                uint64_t *p = (uint64_t*)&dst_info->progbits.content[rela->r_offset];
                *p = offset + rela->r_addend;
              }
              break;
            case STB_GLOBAL:
              {
                const char *label = &strinfo->strtab.buf[sym->st_name];

                ElfObj *telfobj;
                const Elf64_Sym *tsym = find_symbol_from_all(elfobjs, alloc_name(label, NULL, false), &telfobj);
                assert(tsym != NULL && tsym->st_shndx > 0);

                const Elf64_Shdr *tshdr = &telfobj->shdrs[tsym->st_shndx];
                assert(tshdr->sh_type == SHT_PROGBITS || tshdr->sh_type == SHT_NOBITS);
                intptr_t offset = telfobj->section_infos[tsym->st_shndx].progbits.address;
                // intptr_t current = elfobj->section_infos[shdr->sh_info].progbits.address;
                uint64_t *p = (uint64_t*)&dst_info->progbits.content[rela->r_offset];
                *p = (offset + tsym->st_value) + rela->r_addend;
              }
              break;
            default: assert(false); break;
            }
          }
          break;
        case R_X86_64_PC32:
          {
            const Elf64_Sym *sym = &symhdrinfo->symtab.symtabs[ELF64_R_SYM(rela->r_info)];
            switch (ELF64_ST_BIND(sym->st_info)) {
            case STB_LOCAL:
              {
                assert(ELF64_R_SYM(rela->r_info) < elfobj->ehdr.e_shnum);
                const Elf64_Shdr *tshdr = &elfobj->shdrs[ELF64_R_SYM(rela->r_info)];
                assert(tshdr->sh_type == SHT_PROGBITS || tshdr->sh_type == SHT_NOBITS);

                const SectionInfo *s = &elfobj->section_infos[ELF64_R_SYM(rela->r_info)];
                intptr_t offset = s->progbits.address;
                intptr_t current = elfobj->section_infos[shdr->sh_info].progbits.address;
                uint32_t *p = (uint32_t*)&dst_info->progbits.content[rela->r_offset];
                *p = offset - (current + rela->r_offset) + rela->r_addend;
              }
              break;
            case STB_GLOBAL:
              {
                const char *label = &strinfo->strtab.buf[sym->st_name];

                ElfObj *telfobj;
                const Elf64_Sym *tsym = find_symbol_from_all(elfobjs, alloc_name(label, NULL, false), &telfobj);
                assert(tsym != NULL && tsym->st_shndx > 0);

                const Elf64_Shdr *tshdr = &telfobj->shdrs[tsym->st_shndx];
                assert(tshdr->sh_type == SHT_PROGBITS || tshdr->sh_type == SHT_NOBITS);
                intptr_t offset = telfobj->section_infos[tsym->st_shndx].progbits.address;
                intptr_t current = elfobj->section_infos[shdr->sh_info].progbits.address;
                uint32_t *p = (uint32_t*)&dst_info->progbits.content[rela->r_offset];
                *p = (offset + tsym->st_value) - (current + rela->r_offset) + rela->r_addend;
              }
              break;
            default: assert(false); break;
            }
          }
          break;
        case R_X86_64_PLT32:
          {
            const Elf64_Sym *sym = &symhdrinfo->symtab.symtabs[ELF64_R_SYM(rela->r_info)];
            const char *label = &strinfo->strtab.buf[sym->st_name];

            ElfObj *telfobj;
            const Elf64_Sym *tsym = find_symbol_from_all(elfobjs, alloc_name(label, NULL, false), &telfobj);
            assert(tsym != NULL && tsym->st_shndx > 0);

            const Elf64_Shdr *tshdr = &telfobj->shdrs[tsym->st_shndx];
            assert(tshdr->sh_type == SHT_PROGBITS);
            intptr_t offset = telfobj->section_infos[tsym->st_shndx].progbits.address;
            intptr_t current = elfobj->section_infos[shdr->sh_info].progbits.address;
            uint32_t *p = (uint32_t*)&dst_info->progbits.content[rela->r_offset];
            *p = (offset + tsym->st_value) - (current + rela->r_offset) + rela->r_addend;
          }
          break;
        default: assert(false); break;
        }
      }
    }
  }
}

static bool link_elfobjs(Vector *elfobjs, const Name *entry, uintptr_t start_address) {
  Table unresolved;
  table_init(&unresolved);
  table_put(&unresolved, entry, (void*)entry);

  uintptr_t offsets[SEC_BSS + 1];
  Vector *progbit_sections[SEC_BSS + 1];
  for (int secno = 0; secno < SEC_BSS + 1; ++secno) {
    offsets[secno] = 0;
    progbit_sections[secno] = new_vector();
  }

  for (int i = 0; i < elfobjs->len; ++i) {
    ElfObj *elfobj = elfobjs->data[i];
    for (Elf64_Half sec = 0; sec < elfobj->ehdr.e_shnum; ++sec) {
      Elf64_Shdr *shdr = &elfobj->shdrs[sec];
      switch (shdr->sh_type) {
      case SHT_PROGBITS:
        {
          Elf64_Xword size = shdr->sh_size;
          if (size <= 0)
            break;
          void *buf = read_from(elfobj->fp, shdr->sh_offset, size);
          if (buf == NULL) {
            perror("read error");
          }
          enum SectionType secno;
          if (shdr->sh_flags & SHF_EXECINSTR) {
            secno = SEC_CODE;
          } else if (shdr->sh_flags & SHF_WRITE) {
            secno = SEC_DATA;
          } else {
            secno = SEC_RODATA;
          }
          Elf64_Xword align = shdr->sh_addralign;
          align_section_size(secno, align);

          uintptr_t address = ALIGN(offsets[secno], align);
          SectionInfo *p = &elfobj->section_infos[sec];
          p->progbits.address = address;
          p->progbits.content = buf;
          vec_push(progbit_sections[secno], p);
          offsets[secno] = address + size;
        }
        break;
      case SHT_NOBITS:
        {
          Elf64_Xword size = shdr->sh_size;
          if (size <= 0)
            break;
          Elf64_Xword align = shdr->sh_addralign;
          align_section_size(SEC_BSS, align);
          add_bss(size);

          uintptr_t address = ALIGN(offsets[SEC_BSS], align);
          SectionInfo *p = &elfobj->section_infos[sec];
          p->progbits.address = address;
          p->progbits.content = NULL;
          vec_push(progbit_sections[SEC_BSS], p);
          offsets[SEC_BSS] = address + size;
        }
        break;
      case SHT_SYMTAB:
        {
          SectionInfo *p = &elfobj->section_infos[sec];
          SectionInfo *q = &elfobj->section_infos[shdr->sh_link];  // Strtab
          Table *names = &p->symtab.names;
          const char *str = q->strtab.buf;
          const Name *name;
          Elf64_Sym *sym;
          for (int it = 0; (it = table_iterate(names, it, &name, (void**)&sym)) != -1; ) {
            unsigned char type = sym->st_info & 0x0f;
            if (type != STT_NOTYPE || str[sym->st_name] == '\0')
              continue;
            const Name *name = alloc_name(&str[sym->st_name], NULL, false);
            if (sym->st_shndx == 0) {
              if (find_symbol_from_all(elfobjs, name, NULL) == NULL)
                table_put(&unresolved, name, (void *)name);
            } else {
              table_delete(&unresolved, name);
            }
          }
        }
        break;
      default: break;
      }
    }
  }

  if (unresolved.count > 0) {
    fprintf(stderr, "Unresolved: #%d\n", unresolved.count);
    const Name *name;
    void *dummy;
    for (int it = 0; (it = table_iterate(&unresolved, it, &name, &dummy)) != -1;) {
      fprintf(stderr, "  %.*s\n", name->bytes, name->chars);
    }
    return false;
  }

  // Calculate address.
  {
    uintptr_t address = start_address;
    for (int secno = 0; secno < SEC_BSS + 1; ++secno) {
      address = ALIGN(address, section_aligns[secno]);
      Vector *v = progbit_sections[secno];
      if (v->len > 0) {
        for (int i = 0; i < v->len; ++i) {
          SectionInfo *p = v->data[i];
          p->progbits.address += address;
        }
        SectionInfo *last = v->data[v->len - 1];
        address = last->progbits.address + last->shdr->sh_size;
      }
    }
  }

  resolve_relas(elfobjs);

  for (int i = 0; i < elfobjs->len; ++i) {
    ElfObj *elfobj = elfobjs->data[i];
    for (Elf64_Half sec = 0; sec < elfobj->ehdr.e_shnum; ++sec) {
      Elf64_Shdr *shdr = &elfobj->shdrs[sec];
      switch (shdr->sh_type) {
      case SHT_PROGBITS:
        {
          const SectionInfo *p = &elfobj->section_infos[sec];
          void *content = p->progbits.content;
          if (content == NULL)
            continue;
          enum SectionType secno;
          if (shdr->sh_flags & SHF_EXECINSTR) {
            secno = SEC_CODE;
          } else if (shdr->sh_flags & SHF_WRITE) {
            secno = SEC_DATA;
          } else {
            secno = SEC_RODATA;
          }
          align_section_size(secno, shdr->sh_addralign);
          add_section_data(secno, content, shdr->sh_size);
        }
        break;
      default: break;
      }
    }
  }

  return true;
}

static bool output_exe(const char *ofn, Vector *elfobjs, const Name *entry) {
  size_t codesz, rodatasz, datasz, bsssz;
  uintptr_t codeloadadr, dataloadadr;
  get_section_size(SEC_CODE, &codesz, &codeloadadr);
  get_section_size(SEC_RODATA, &rodatasz, NULL);
  get_section_size(SEC_DATA, &datasz, &dataloadadr);
  get_section_size(SEC_BSS, &bsssz, NULL);

  ElfObj *telfobj;
  const Elf64_Sym *tsym = find_symbol_from_all(elfobjs, entry, &telfobj);
  if (tsym == NULL)
    error("Cannot find label: `%.*s'", entry->bytes, entry->chars);
  uintptr_t entry_address = telfobj->section_infos[tsym->st_shndx].progbits.address + tsym->st_value;

  int phnum = datasz > 0 || bsssz > 0 ? 2 : 1;

  FILE *fp;
  if (ofn == NULL) {
    fp = stdout;
  } else {
    fp = fopen(ofn, "wb");
    if (fp == NULL) {
      fprintf(stderr, "Failed to open output file: %s\n", ofn);
      return false;
    }
  }

  size_t rodata_align = MAX(section_aligns[SEC_RODATA], 1);
  size_t code_rodata_sz = ALIGN(codesz, rodata_align) + rodatasz;
  out_elf_header(fp, entry_address, phnum, 0);
  out_program_header(fp, 0, PROG_START, codeloadadr, code_rodata_sz, code_rodata_sz);
  if (phnum > 1) {
    size_t bss_align = MAX(section_aligns[SEC_BSS], 1);
    size_t datamemsz = ALIGN(datasz, bss_align) + bsssz;
    out_program_header(fp, 1, ALIGN(PROG_START + code_rodata_sz, DATA_ALIGN), dataloadadr, datasz,
                       datamemsz);
  }

  uintptr_t addr = PROG_START;
  put_padding(fp, addr);
  output_section(fp, SEC_CODE);
  addr += codesz;
  if (rodatasz > 0) {
    size_t rodata_align = MAX(section_aligns[SEC_RODATA], 1);
    addr = ALIGN(addr, rodata_align);
    put_padding(fp, addr);
    output_section(fp, SEC_RODATA);
    addr += rodatasz;
  }
  if (datasz > 0) {
    addr = ALIGN(addr, DATA_ALIGN);
    put_padding(fp, addr);
    output_section(fp, SEC_DATA);
    addr += datasz;
  }
  fclose(fp);

#if !defined(__XV6)
  if (chmod(ofn, 0755) == -1) {
    perror("chmod failed\n");
    return false;
  }
#endif
  return true;
}

int main(int argc, char *argv[]) {
  const char *ofn = NULL;
  const char *entry = kDefaultEntryName;

  struct option longopts[] = {
    {"version", no_argument, NULL, 'V'},
    {0},
  };
  int opt;
  int longindex;
  while ((opt = getopt_long(argc, argv, "Vo:e:", longopts, &longindex)) != -1) {
    switch (opt) {
    case 'V':
      show_version("ld");
      break;
    case 'o':
      ofn = optarg;
      break;
    case 'e':
      entry = optarg;
      break;
    default:
      fprintf(stderr, "Unknown option: %s\n", argv[optind]);
      return 1;
    }
  }

  int iarg = optind;
  if (iarg >= argc)
    error("no input");

  if (ofn == NULL)
    ofn = "a.out";

  section_aligns[SEC_DATA] = DATA_ALIGN;

  Vector *elfobjs = new_vector();
  for (int i = iarg; i < argc; ++i) {
    ElfObj *elfobj = malloc_or_die(sizeof(*elfobj));
    elfobj_init(elfobj);
    if (!open_elf(argv[i], elfobj))
      return 1;
    vec_push(elfobjs, elfobj);
  }

  const Name *entry_name = alloc_name(entry, NULL, false);
  bool result = link_elfobjs(elfobjs, entry_name, LOAD_ADDRESS);
  if (result) {
    fix_section_size(LOAD_ADDRESS);
    result = output_exe(ofn, elfobjs, entry_name);
  }

  for (int i = 0; i < elfobjs->len; ++i) {
    close_elf(elfobjs->data[i]);
  }
  return result ? 0 : 1;
}
