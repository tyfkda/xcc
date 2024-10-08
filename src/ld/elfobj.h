#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#if defined(__linux__)
#include <elf.h>
#else
#include "../../include/elf.h"
#endif

typedef struct Name Name;
typedef struct Table Table;
typedef struct Vector Vector;

typedef struct ElfSectionInfo {
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
      Elf64_Sym *syms;
      struct ElfSectionInfo *strtab;
      size_t count;
    } symtab;
  };
} ElfSectionInfo;

typedef struct ElfObj {
  FILE *fp;
  size_t start_offset;
  Elf64_Ehdr ehdr;
  Elf64_Shdr *shdrs;
  Table *symbol_table;  // <Elf64_Sym*>, global only

  Vector *prog_sections;  // <ElfSectionInfo*>, PROGBITS, NOBITS, INIT_ARRAY, FINI_ARRAY, PREINIT_ARRAY
  ElfSectionInfo *section_infos;
  ElfSectionInfo *symtab_section;
  int nobit_shndx;
} ElfObj;

ElfObj *read_elf(FILE *fp, const char *fn);
void close_elf(ElfObj *elfobj);
Elf64_Sym *elfobj_find_symbol(ElfObj *elfobj, const Name *name);
