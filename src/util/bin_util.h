// Binary format utility

#pragma once

#include "../config.h"

#include <stdio.h>  // FILE
#include <stddef.h>  // size_t

#include "table.h"

#ifndef ELF_NOT_SUPPORTED
#ifdef __APPLE__
#include "../../include/elf.h"
#else
#include <elf.h>
#endif
#endif

// String table.
typedef struct {
  Table offsets;
  size_t size;
} Strtab;

void strtab_init(Strtab *strtab);
size_t strtab_add(Strtab *strtab, const Name *name);
void *strtab_dump(const Strtab *strtab);

// Symtab.

typedef struct {
  Strtab strtab;
  Table indices;
  void *buf;  // Elf64_Sym *, or struct nlist_64 *
  int count;
#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
  int ilocalsym, iextdefsym, iundefsym;  // for dysymtab.
#endif
} Symtab;

void symtab_init(Symtab *symtab);
int symtab_find(Symtab *symtab, const Name *name);
void *symtab_add(Symtab *symtab, const Name *name);
void symtab_concat(Symtab *dest, Symtab *src);

// ELF.
void out_elf_header(FILE *fp, uintptr_t entry, int phnum, int shnum, int flags, uintptr_t shoff);
void out_program_header(FILE *fp, uintptr_t offset, uintptr_t vaddr, size_t filesz, size_t memsz,
                        int flags);
