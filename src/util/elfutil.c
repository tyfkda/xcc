#include "../config.h"
#include "elfutil.h"

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
    .p_paddr  = vaddr, // dummy
    .p_filesz = filesz,
    .p_memsz  = memsz,
    .p_flags  = kFlags[sec],
    .p_align  = 0x1000,
  };

  fwrite(&phdr, sizeof(Elf64_Phdr), 1, fp);
}
#endif  // !ELF_NOT_SUPPORTED
