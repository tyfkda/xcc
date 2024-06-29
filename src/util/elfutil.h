// ELF format utility

#pragma once

#include <stdio.h>  // FILE

#ifdef __APPLE__
#include "../../include/elf.h"
#else
#include <elf.h>
#endif

void out_elf_header(FILE *fp, uintptr_t entry, int phnum, int shnum, int flags);
void out_program_header(FILE *fp, int sec, uintptr_t offset, uintptr_t vaddr, size_t filesz,
                        size_t memsz);
