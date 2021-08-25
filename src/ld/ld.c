#include <assert.h>
#include <elf.h>
#include <getopt.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "table.h"
#include "util.h"

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

static char *shtypename(int shtype) {
  switch (shtype) {
  case SHT_NULL: return "null";
  case SHT_PROGBITS: return "progbits";
  case SHT_SYMTAB: return "symtab";
  case SHT_STRTAB: return "strtab";
  case SHT_RELA: return "rela";
  case SHT_NOBITS: return "nobits";
  default:
    {
      static char buf[32];
      sprintf(buf, "type%d", shtype);
      return buf;
    }
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
  FILE *fp;
  Elf64_Ehdr ehdr;
  Elf64_Shdr *shdrs;
  char *shstrtab;

  struct {
    Elf64_Shdr *shdr;
    Elf64_Sym *symtabs;
    Table names;  // <Elf64_Sym*>
  } symtab;
} ElfObj;

static void elfobj_init(ElfObj *elfobj) {
  memset(elfobj, 0, sizeof(*elfobj));
  table_init(&elfobj->symtab.names);
}

static int find_section(ElfObj *elfobj, unsigned int section_type) {
  for (int i = 0; i < elfobj->ehdr.e_shnum; ++i) {
    Elf64_Shdr *p = &elfobj->shdrs[i];
    if (p->sh_type == section_type)
      return i;
  }
  return -1;
}

static bool load_symtab(ElfObj *elfobj) {
  int symtab_index = find_section(elfobj, SHT_SYMTAB);
  if (symtab_index < 0)
    return false;

  Elf64_Shdr *symtab_sec = &elfobj->shdrs[symtab_index];
  Elf64_Sym *symtabs = read_from(elfobj->fp, symtab_sec->sh_offset, symtab_sec->sh_size);
  if (symtabs == NULL)
    perror("read symtab failed");
  if (symtab_sec->sh_size % sizeof(Elf64_Sym) != 0)
    error("malformed symtab");

  Elf64_Shdr *strtab_sec = &elfobj->shdrs[symtab_sec->sh_link];
  const char *str = read_strtab(elfobj->fp, strtab_sec);
  if (str == NULL)
    error("read strtab failed");

  Table *names = &elfobj->symtab.names;
  size_t count = symtab_sec->sh_size / sizeof(Elf64_Sym);
  for (uint32_t i = 0; i < count; ++i) {
    Elf64_Sym *sym = &symtabs[i];
    unsigned char type = sym->st_info & 0x0f;
    if (type == STT_NOTYPE && str[sym->st_name] != '\0') {
      const Name *name = alloc_name(&str[sym->st_name], NULL, false);
      table_put(names, name, sym);
    }
  }

  elfobj->symtab.shdr = symtab_sec;
  elfobj->symtab.symtabs = symtabs;
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

void dump(FILE *fp, ElfObj *elfobj) {
  for (int i = 0; i < elfobj->ehdr.e_shnum; ++i) {
    Elf64_Shdr *p = &elfobj->shdrs[i];
    fprintf(fp, "%2d:%-16s type=%8s, offset=%lx, size=%lx\n", i, &elfobj->shstrtab[p->sh_name], shtypename(p->sh_type), p->sh_offset, p->sh_size);
  }

  {
    printf("Symbols: #%d\n", elfobj->symtab.names.count);
    const Name *name;
    Elf64_Sym *sym;
    for (int it = 0; (it = table_iterate(&elfobj->symtab.names, it, &name, (void**)&sym)) != -1; ) {
      unsigned char bind = sym->st_info >> 4;
      char *bindname;
      switch (bind) {
      case STB_LOCAL:  bindname = "LOCAL"; break;
      case STB_GLOBAL:  bindname = "GLOBAL"; break;
      default:  bindname = "?"; break;
      }
      printf("  %.*s: bind=%s\n", name->bytes, name->chars, bindname);
    }
  }
}

int main(int argc, char *argv[]) {
  const char *ofn = NULL;

  struct option longopts[] = {
    {"version", no_argument, NULL, 'V'},
    {0},
  };
  int opt;
  int longindex;
  while ((opt = getopt_long(argc, argv, "Vo:", longopts, &longindex)) != -1) {
    switch (opt) {
    case 'V':
      show_version("ld");
      break;
    case 'o':
      ofn = optarg;
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

  for (int i = iarg; i < argc; ++i) {
    ElfObj elfobj;
    elfobj_init(&elfobj);
    if (!open_elf(argv[i], &elfobj))
      return 1;
    dump(stdout, &elfobj);
    close_elf(&elfobj);
  }

  return 0;
}
