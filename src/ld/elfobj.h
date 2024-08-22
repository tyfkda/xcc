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
    } symtab;
  };
} ElfSectionInfo;

typedef struct ElfObj {
  FILE *fp;
  size_t start_offset;
  Elf64_Ehdr ehdr;
  Elf64_Shdr *shdrs;
  Table *symbol_table;  // <Elf64_Sym*>, global only

  ElfSectionInfo *section_infos;
  ElfSectionInfo *symtab_section;
} ElfObj;

ElfObj *read_elf(FILE *fp, const char *fn);
void close_elf(ElfObj *elfobj);
Elf64_Sym *elfobj_find_symbol(ElfObj *elfobj, const Name *name);
