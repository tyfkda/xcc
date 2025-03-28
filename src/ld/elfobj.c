#include "../config.h"
#include "elfobj.h"

#include <assert.h>
#include <stdlib.h>  // free
#include <string.h>

#include "table.h"
#include "util.h"

static Elf64_Shdr *read_all_section_headers(FILE *fp, size_t start_offset, Elf64_Ehdr *ehdr) {
  if (ehdr->e_shnum <= 0)
    return NULL;
  return read_or_die(fp, NULL, ehdr->e_shoff + start_offset,
                     ehdr->e_shnum * sizeof(Elf64_Shdr), "read section header failed");
}

static char *read_strtab(FILE *fp, size_t start_offset, Elf64_Shdr *sec) {
  assert(sec->sh_type == SHT_STRTAB);
  return read_or_die(fp, NULL, sec->sh_offset + start_offset, sec->sh_size, "read strtab failed");
}

static void load_symtab(ElfObj *elfobj) {
  for (Elf64_Half sec = 0; sec < elfobj->ehdr.e_shnum; ++sec) {
    Elf64_Shdr *shdr = &elfobj->shdrs[sec];
    if (shdr->sh_type != SHT_SYMTAB)
      continue;
    if (elfobj->symtab_section != NULL)
      error("Multiple symtabs not supported\n");
    if (shdr->sh_size % sizeof(Elf64_Sym) != 0)
      error("illega symtab size");

    Elf64_Sym *symbols = read_or_die(elfobj->fp, NULL, shdr->sh_offset + elfobj->start_offset,
                                     shdr->sh_size, "read symtab failed");

    ElfSectionInfo *p = &elfobj->section_infos[sec];
    p->symtab.syms = symbols;
    ElfSectionInfo *strtab_section = &elfobj->section_infos[shdr->sh_link];
    p->symtab.strtab = strtab_section;
    p->symtab.count = shdr->sh_size / sizeof(Elf64_Sym);
    elfobj->symtab_section = p;

    // Check strtab.
    Elf64_Shdr *strtab_sec = strtab_section->shdr;
    if (strtab_sec->sh_type != SHT_STRTAB)
      error("malformed symtab");

    const char *strbuf = read_strtab(elfobj->fp, elfobj->start_offset, strtab_sec);
    if (strbuf == NULL)
      error("read strtab failed");
    elfobj->section_infos[shdr->sh_link].strtab.buf = strbuf;

    // Construct global symbol table.
    assert(elfobj->symbol_table == NULL);
    Table *symbol_table = alloc_table();
    elfobj->symbol_table = symbol_table;
    for (size_t i = 0, count = p->symtab.count; i < count; ++i) {
      Elf64_Sym *sym = &symbols[i];
      unsigned char bind = ELF64_ST_BIND(sym->st_info);
      if (bind == STB_GLOBAL || bind == STB_WEAK) {
        const Name *name = alloc_name(&strbuf[sym->st_name], NULL, false);
        table_put(symbol_table, name, sym);
      }
    }
  }
  if (elfobj->symtab_section == NULL)
    error("no symtab");
}

ElfObj *read_elf(FILE *fp, const char *fn) {
  long start_offset = ftell(fp);
  Elf64_Ehdr ehdr;
  ssize_t size = fread(&ehdr, sizeof(ehdr), 1, fp);
  if (size != 1 || ehdr.e_ident[0] != ELFMAG0 || ehdr.e_ident[1] != ELFMAG1 ||
      ehdr.e_ident[2] != ELFMAG2 || ehdr.e_ident[3] != ELFMAG3) {
    fprintf(stderr, "no elf file: %s\n", fn);
    return NULL;
  }
  if (ehdr.e_machine != MACHINE_TYPE || ehdr.e_version != EV_CURRENT ||
      ehdr.e_ehsize != sizeof(Elf64_Ehdr) ||
      ehdr.e_shentsize != sizeof(Elf64_Shdr) ||
      ehdr.e_shnum < 1 || ehdr.e_shstrndx >= ehdr.e_shnum) {
    fprintf(stderr, "illegal elf: %s\n", fn);
    return NULL;
  }
  Elf64_Shdr *shdrs = read_all_section_headers(fp, start_offset, &ehdr);
  if (shdrs == NULL)
    return NULL;

  ElfObj *elfobj = calloc_or_die(sizeof(*elfobj));
  elfobj->fp = fp;
  elfobj->start_offset = start_offset;
  elfobj->ehdr = ehdr;
  elfobj->shdrs = shdrs;
  elfobj->symbol_table = NULL;
  elfobj->symtab_section = NULL;
  elfobj->nobit_shndx = -1;

  Vector *prog_sections = new_vector();
  ElfSectionInfo *section_infos = calloc_or_die(ehdr.e_shnum * sizeof(ElfSectionInfo));
  elfobj->section_infos = section_infos;
  for (unsigned short i = 0; i < ehdr.e_shnum; ++i) {
    Elf64_Shdr *shdr = &shdrs[i];
    ElfSectionInfo *p = &section_infos[i];
    p->elfobj = elfobj;
    p->shdr = shdr;
    if (shdr->sh_size > 0) {
      switch (shdr->sh_type) {
      case SHT_NOBITS:
        if (elfobj->nobit_shndx >= 0)
          error("Multiple NOBITS sections not supported");
        elfobj->nobit_shndx = i;
        // Fallthrough
      case SHT_PROGBITS:
      case SHT_INIT_ARRAY:
      case SHT_FINI_ARRAY:
      case SHT_PREINIT_ARRAY:
        vec_push(prog_sections, p);
        break;
      default: break;
      }
    }
  }
  elfobj->prog_sections = prog_sections;

  load_symtab(elfobj);

  {
    ElfSectionInfo *shstrtab = &section_infos[ehdr.e_shstrndx];
    if (shstrtab->shdr->sh_type != SHT_STRTAB) {
      fprintf(stderr, "illegal shstrtab: %s\n", fn);
      return NULL;
    }
    if (shstrtab->strtab.buf == NULL) {
      const char *buf = read_strtab(elfobj->fp, elfobj->start_offset, shstrtab->shdr);
      if (buf == NULL)
        error("read shstrtab failed");
      shstrtab->strtab.buf = buf;
    }
  }

  return elfobj;
}

void close_elf(ElfObj *elfobj) {
  if (elfobj->fp != NULL) {
    fclose(elfobj->fp);
    elfobj->fp = NULL;
  }
}

Elf64_Sym *elfobj_find_symbol(ElfObj *elfobj, const Name *name) {
  Elf64_Sym *sym = table_get(elfobj->symbol_table, name);
  return (sym != NULL && sym->st_shndx != SHN_UNDEF) ? sym : NULL;
}
