#include "../config.h"
#include "bin_util.h"

#include <assert.h>
#include <string.h>  // memcpy

#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
#include <mach-o/nlist.h>  // nlist_64
#endif

#include "util.h"

// strtab

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

void *strtab_dump(const Strtab *strtab) {
  void *buf = malloc_or_die(strtab->size);
  unsigned char *p = buf;
  const Name *name;
  void *value;
  for (int it = 0; (it = table_iterate(&strtab->offsets, it, &name, &value)) != -1; ) {
    uintptr_t offset = VOIDP2UINT(value);
    memcpy(p + offset, name->chars, name->bytes);
    p[offset + name->bytes] = '\0';
  }
  return buf;
}

// Symtab.

#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
typedef struct nlist_64  SymbolType;
#define SET_SYMBOL_NAME_OFFSET(sym, offset)  (sym->n_un.n_strx = offset)
#else
typedef Elf64_Sym  SymbolType;
#define SET_SYMBOL_NAME_OFFSET(sym, offset)  (sym->st_name = offset)
#endif

void symtab_init(Symtab *symtab) {
  strtab_init(&symtab->strtab);
  table_init(&symtab->indices);
  symtab->buf = NULL;
  symtab->count = 0;
}

int symtab_find(Symtab *symtab, const Name *name) {
  intptr_t index;
  if (table_try_get(&symtab->indices, name, (void**)&index))
    return index;
  return -1;
}

void *symtab_add(Symtab *symtab, const Name *name) {
  uint32_t offset = strtab_add(&symtab->strtab, name);
  if (name->bytes > 0) {
    int index = symtab_find(symtab, name);
    if (index >= 0)
      return ((SymbolType*)symtab->buf) + index;
  }

  int old_count = symtab->count;
  int new_count = old_count + 1;
  SymbolType *newbuf = realloc_or_die(symtab->buf, sizeof(*newbuf) * new_count);
  symtab->buf = newbuf;
  symtab->count = new_count;
  SymbolType *sym = &newbuf[old_count];
  memset(sym, 0x00, sizeof(*sym));
  SET_SYMBOL_NAME_OFFSET(sym, offset);
  table_put(&symtab->indices, name, INT2VOIDP(old_count));
  return sym;
}

void symtab_concat(Symtab *dest, Symtab *src) {
  int n = src->count;
  const Name **names = alloca(sizeof(*names) * n);
  const Name *name;
  intptr_t index;
  for (int it = 0; (it = table_iterate(&src->indices, it, &name, (void**)&index)) != -1; ) {
    assert(index < n);
    names[index] = name;
  }
  Elf64_Sym *buf = src->buf;
  for (int i = 0; i < n; ++i) {
    const Name *name = names[i];
    Elf64_Sym *p = symtab_add(dest, name);
    Elf64_Word st_name_bak = p->st_name;
    memcpy(p, &buf[i], sizeof(*p));
    p->st_name = st_name_bak;
  }
}

// ELF.

#ifndef ELF_NOT_SUPPORTED
void out_elf_header(FILE *fp, uintptr_t entry, int phnum, int shnum, int flags, uintptr_t shoff) {
  Elf64_Ehdr ehdr = {
    .e_ident     = { ELFMAG0, ELFMAG1, ELFMAG2 ,ELFMAG3,
                     ELFCLASS64, ELFDATA2LSB, EV_CURRENT, ELFOSABI_SYSV },
    .e_type      = phnum > 0 ? ET_EXEC : ET_REL,
    .e_machine   = MACHINE_TYPE,
    .e_version   = EV_CURRENT,
    .e_entry     = entry,
    .e_phoff     = phnum > 0 ? sizeof(Elf64_Ehdr) : 0,
    .e_shoff     = shoff,
    .e_flags     = flags,
    .e_ehsize    = sizeof(Elf64_Ehdr),
    .e_phentsize = phnum > 0 ? sizeof(Elf64_Phdr) : 0,
    .e_phnum     = phnum,
    .e_shentsize = shnum > 0 ? sizeof(Elf64_Shdr) : 0,
    .e_shnum     = shnum,
    .e_shstrndx  = shnum > 0 ? shnum - 1 : SHN_UNDEF,  // Assumes shstrndx is at last.
  };

  fwrite(&ehdr, sizeof(Elf64_Ehdr), 1, fp);
}

void out_program_header(FILE *fp, uintptr_t offset, uintptr_t vaddr, size_t filesz, size_t memsz,
                        int flags) {
  Elf64_Phdr phdr = {
    .p_type   = PT_LOAD,
    .p_offset = offset,
    .p_vaddr  = vaddr,
    .p_paddr  = vaddr, // dummy
    .p_filesz = filesz,
    .p_memsz  = memsz,
    .p_flags  = flags,
    .p_align  = 0x1000,
  };

  fwrite(&phdr, sizeof(Elf64_Phdr), 1, fp);
}
#endif  // !ELF_NOT_SUPPORTED
