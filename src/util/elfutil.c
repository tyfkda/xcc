#include "../config.h"
#include "elfutil.h"

#ifndef ELF_NOT_SUPPORTED

#include <stdio.h>
#include <stdlib.h>  // realloc
#include <string.h>  // memcpy

#include "util.h"

void strtab_init(Strtab *strtab) {
  table_init(&strtab->offsets);
  strtab->size = 0;
}

size_t strtab_add(Strtab *strtab, const Name *name) {
  void *result;
  if (!table_try_get(&strtab->offsets, name, &result)) {
    size_t offset = strtab->size;
    table_put(&strtab->offsets, name, (void*)offset);
    strtab->size += name->bytes + 1;
    return offset;
  } else {
    return (size_t)result;
  }
}

void *strtab_dump(Strtab *strtab) {
  void *buf = malloc_or_die(strtab->size);
  unsigned char *p = buf;
  const Name *name;
  void *value;
  for (int it = 0; (it = table_iterate(&strtab->offsets, it, &name, &value)) != -1; ) {
    uintptr_t offset = (uintptr_t)value;
    memcpy(p + offset, name->chars, name->bytes);
    p[offset + name->bytes] = '\0';
  }
  return buf;
}

//

void symtab_init(Symtab *symtab) {
  strtab_init(&symtab->strtab);
  table_init(&symtab->indices);
  symtab->buf = NULL;
  symtab->count = 0;
}

Elf64_Sym *symtab_add(Symtab *symtab, const Name *name) {
  uint32_t offset = strtab_add(&symtab->strtab, name);
  if (name->bytes > 0) {
    for (int i = 0; i < symtab->count; ++i) {
      uintptr_t index;
      if (table_try_get(&symtab->indices, name, (void**)&index)) {
        return &symtab->buf[index];
      }
    }
  }

  int old_count = symtab->count;
  int new_count = old_count + 1;
  symtab->buf = realloc_or_die(symtab->buf, sizeof(*symtab->buf) * new_count);;
  symtab->count = new_count;
  Elf64_Sym *sym = &symtab->buf[old_count];
  memset(sym, 0x00, sizeof(*sym));
  sym->st_name = offset;
  table_put(&symtab->indices, name, (void*)(uintptr_t)old_count);
  return sym;
}

//

void out_elf_header(FILE *fp, uintptr_t entry, int phnum, int shnum) {
  Elf64_Ehdr ehdr = {
    .e_ident     = { ELFMAG0, ELFMAG1, ELFMAG2 ,ELFMAG3,
                     ELFCLASS64, ELFDATA2LSB, EV_CURRENT, ELFOSABI_SYSV },
    .e_type      = phnum > 0 ? ET_EXEC : ET_REL,
    .e_machine   = MACHINE_TYPE,
    .e_version   = EV_CURRENT,
    .e_entry     = entry,
    .e_phoff     = phnum > 0 ? sizeof(Elf64_Ehdr) : 0,
    .e_shoff     = 0, // dummy
    .e_flags     = 0x0,
    .e_ehsize    = sizeof(Elf64_Ehdr),
    .e_phentsize = phnum > 0 ? sizeof(Elf64_Phdr) : 0,
    .e_phnum     = phnum,
    .e_shentsize = shnum > 0 ? sizeof(Elf64_Shdr) : 0,
    .e_shnum     = shnum,
    .e_shstrndx  = shnum > 0 ? shnum - 1 : 0,
  };

  fwrite(&ehdr, sizeof(Elf64_Ehdr), 1, fp);
}

void out_program_header(FILE *fp, int sec, uintptr_t offset, uintptr_t vaddr, size_t filesz,
                        size_t memsz) {
  static const int kFlags[] = {
    PF_R | PF_X,  // code
    PF_R | PF_W,  // rwdata
  };

  Elf64_Phdr phdr = {
    .p_type   = PT_LOAD,
    .p_offset = offset,
    .p_vaddr  = vaddr,
    .p_paddr  = 0, // dummy
    .p_filesz = filesz,
    .p_memsz  = memsz,
    .p_flags  = kFlags[sec],
    .p_align  = 0x10,
  };

  fwrite(&phdr, sizeof(Elf64_Phdr), 1, fp);
}

#else
// Avoid: error: ISO C requires a translation unit to contain at least one declaration [-Werror,-Wempty-translation-unit]
typedef int make_iso_compilers_happy;
#endif  // !ELF_NOT_SUPPORTED
