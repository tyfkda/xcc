#include <elf.h>
#include <getopt.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "util.h"

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
  Elf64_Shdr *esecs = malloc(sizeof(Elf64_Shdr) * ehdr->e_shnum);
  if (esecs == NULL) {
    fprintf(stderr, "memory overflow\n");
  } else {
    fseek(fp, ehdr->e_shoff, SEEK_SET);
    if (fread(esecs, sizeof(Elf64_Shdr), ehdr->e_shnum, fp) != ehdr->e_shnum) {
      fprintf(stderr, "read section header failed\n");
    } else {
      return esecs;
    }
    free(esecs);
  }
  return NULL;
}

static char *read_strtab(FILE *fp, Elf64_Shdr *sec) {
  if (sec->sh_type != SHT_STRTAB)
    return NULL;
  size_t sz = sec->sh_size;
  char *buf = malloc(sz);
  if (buf == NULL) {
    fprintf(stderr, "memory overflow\n");
  } else {
    fseek(fp, sec->sh_offset, SEEK_SET);
    if (fread(buf, 1, sz, fp) == sz) {
      return buf;
    } else {
      fprintf(stderr, "read strtab failed\n");
    }
    free(buf);
  }
  return NULL;
}

typedef struct {
  Elf64_Ehdr ehdr;
  Elf64_Shdr *shdrs;
  char *shstrtab;
} ElfObj;

static void elfobj_init(ElfObj *elfobj) {
  memset(elfobj, 0, sizeof(*elfobj));
}

static bool open_elf(const char *fn, ElfObj *elfobj) {
  FILE *fp = fopen(fn, "r");
  if (fp == NULL) {
    fprintf(stderr, "cannot open: %s\n", fn);
    return false;
  }

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

  // Read all section headers.
  elfobj->shdrs = read_all_section_headers(fp, &elfobj->ehdr);
  if (elfobj->shdrs == NULL)
    return false;

  // Read strtab for section header.
  elfobj->shstrtab = read_strtab(fp, &elfobj->shdrs [elfobj->ehdr.e_shstrndx]);
  if (elfobj->shstrtab == NULL)
    return NULL;

  fclose(fp);
  return true;
}

void dump(FILE *fp, ElfObj *elfobj) {
  for (int i = 0; i < elfobj->ehdr.e_shnum; ++i) {
    Elf64_Shdr *p = &elfobj->shdrs[i];
    fprintf(fp, "%2d:%-16s type=%8s, offset=%lx, size=%lx\n", i, &elfobj->shstrtab[p->sh_name], shtypename(p->sh_type), p->sh_offset, p->sh_size);
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
  }

  return 0;
}
