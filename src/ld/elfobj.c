#include "elfobj.h"

#include <assert.h>
#include <stdlib.h>  // calloc
#include <string.h>

#include "table.h"
#include "util.h"

void *malloc_or_die(size_t size) {
  void *p = malloc(size);
  if (p == NULL) {
    fprintf(stderr, "memory overflow\n");
    exit(1);
  }
  return p;
}

void *read_from(FILE *fp, unsigned long offset, size_t size) {
  void *buf = malloc_or_die(size);
  if (fseek(fp, offset, SEEK_SET) != 0 ||
      fread(buf, 1, size, fp) != size) {
    free(buf);
    buf = NULL;
  }
  return buf;
}

static Elf64_Shdr *read_all_section_headers(FILE *fp, size_t start_offset, Elf64_Ehdr *ehdr) {
  if (ehdr->e_shnum <= 0)
    return NULL;
  Elf64_Shdr *esecs = read_from(fp, ehdr->e_shoff + start_offset, ehdr->e_shnum * sizeof(Elf64_Shdr));
  if (esecs == NULL) {
    perror("read section header failed");
  }
  return esecs;
}

static char *read_strtab(FILE *fp, size_t start_offset, Elf64_Shdr *sec) {
  assert(sec->sh_type == SHT_STRTAB);
  char *buf = read_from(fp, sec->sh_offset + start_offset, sec->sh_size);
  if (buf == NULL) {
    perror("read strtab failed");
  }
  return buf;
}

static bool load_symtab(ElfObj *elfobj) {
  for (Elf64_Half sec = 0; sec < elfobj->ehdr.e_shnum; ++sec) {
    Elf64_Shdr *shdr = &elfobj->shdrs[sec];
    switch (shdr->sh_type) {
    case SHT_STRTAB:
      {
        const char *buf = read_strtab(elfobj->fp, elfobj->start_offset, shdr);
        if (buf == NULL)
          error("read strtab failed");
        elfobj->section_infos[sec].strtab.buf = buf;
      }
      break;
    case SHT_SYMTAB:
      {
        Elf64_Sym *symtabs = read_from(elfobj->fp, shdr->sh_offset + elfobj->start_offset, shdr->sh_size);
        if (symtabs == NULL)
          perror("read symtab failed");
        if (shdr->sh_size % sizeof(Elf64_Sym) != 0)
          error("malformed symtab");

        Elf64_Shdr *strtab_sec = &elfobj->shdrs[shdr->sh_link];
        if (strtab_sec->sh_type != SHT_STRTAB)
          error("malformed symtab");

        ElfSectionInfo *p = &elfobj->section_infos[sec];
        p->symtab.names = malloc(sizeof(*p->symtab.names));
        table_init(p->symtab.names);
        p->symtab.symtabs = symtabs;
      }
      break;
    }
  }

  for (Elf64_Half sec = 0; sec < elfobj->ehdr.e_shnum; ++sec) {
    Elf64_Shdr *shdr = &elfobj->shdrs[sec];
    if (shdr->sh_type != SHT_SYMTAB)
      continue;

    ElfSectionInfo *p = &elfobj->section_infos[sec];
    ElfSectionInfo *q = &elfobj->section_infos[shdr->sh_link];  // Strtab
    const char *str = q->strtab.buf;
    Table *names = p->symtab.names;
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

bool read_elf(ElfObj *elfobj, FILE *fp, const char *fn) {
  elfobj->fp = fp;
  elfobj->start_offset = ftell(fp);
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
  elfobj->shdrs = read_all_section_headers(fp, elfobj->start_offset, &elfobj->ehdr);
  if (elfobj->shdrs != NULL) {
    ElfSectionInfo *section_infos = calloc(elfobj->ehdr.e_shnum, sizeof(*elfobj->section_infos));
    elfobj->section_infos = section_infos;
    for (unsigned short i = 0; i < elfobj->ehdr.e_shnum; ++i) {
      Elf64_Shdr *shdr = &elfobj->shdrs[i];
      ElfSectionInfo *p = &section_infos[i];
      p->elfobj = elfobj;
      p->shdr = shdr;
    }

    elfobj->shstrtab = read_strtab(fp, elfobj->start_offset, &elfobj->shdrs[elfobj->ehdr.e_shstrndx]);
    if (elfobj->shstrtab != NULL) {
      if (!load_symtab(elfobj))
        return false;
    }
  }
  return true;
}

void elfobj_init(ElfObj *elfobj) {
  memset(elfobj, 0, sizeof(*elfobj));
}

bool open_elf(const char *fn, ElfObj *elfobj) {
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

void close_elf(ElfObj *elfobj) {
  if (elfobj->fp != NULL) {
    fclose(elfobj->fp);
    elfobj->fp = NULL;
  }
}

Elf64_Sym *elfobj_find_symbol(ElfObj *elfobj, const Name *name) {
  for (Elf64_Half sec = 0; sec < elfobj->ehdr.e_shnum; ++sec) {
    Elf64_Shdr *shdr = &elfobj->shdrs[sec];
    if (shdr->sh_type != SHT_SYMTAB)
      continue;
    ElfSectionInfo *p = &elfobj->section_infos[sec];
    Elf64_Sym *sym = table_get(p->symtab.names, name);
    if (sym != NULL)
      return sym;
  }
  return NULL;
}
