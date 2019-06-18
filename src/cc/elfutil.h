#pragma once

#include <stdio.h>  // FILE
#include <stdint.h>  // ssize_t

void out_elf_header(FILE* fp, uintptr_t entry, int phnum);
void out_program_header(FILE* fp, int sec, uintptr_t offset, uintptr_t vaddr, uintptr_t filesz, uintptr_t memsz);
