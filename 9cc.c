#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#define PROG_START   (0x80)

#if defined(__XV6)
// XV6
#include "../kernel/types.h"
#include "../kernel/elf.h"
#include "../kernel/syscall.h"
#include "../kernel/traps.h"

#define SYSCALL(no)  \
  ADD_CODE(0xb8, (no), (no) >> 8, (no) >> 16, (no) >> 24,  /* mov $EXIT, %eax */ \
           0xcd, T_SYSCALL)                                /* int $64 */

#define SYSCALL_EXIT   (SYS_exit)

#define START_ADDRESS    0x1000

#elif defined(__linux__)
// Linux
#include <elf.h>

#define SYSCALL(no)  \
  ADD_CODE(0xb8, (no), (no) >> 8, (no) >> 16, (no) >> 24,  /* mov $EXIT, %eax */ \
           0x0f, 0x05)                                     /* syscall */

#define SYSCALL_EXIT   (60 /*__NR_exit*/)

#define START_ADDRESS    (0x1000000 + PROG_START)

#else

#error Target not supported

#endif

#define LOAD_ADDRESS    START_ADDRESS

////////////////////////////////////////////////

void error(const char* msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

unsigned char* code;
size_t codesize;

void add_code(const unsigned char* buf, size_t size) {
  size_t newsize = codesize + size;
  code = realloc(code, newsize);
  if (code == NULL)
    error("not enough memory");
  memcpy(code + codesize, buf, size);
  codesize = newsize;
}

#define ADD_CODE(...)  do { unsigned char buf[] = {__VA_ARGS__}; add_code(buf, sizeof(buf)); } while (0)
#define IM32(x)  (x), ((x) >> 8), ((x) >> 16), ((x) >> 24)

#define MOV_I32_EAX(x)   ADD_CODE(0xb8, IM32(x))  // mov $0xNN,%eax
#define MOVSX_EAX_RDI()  ADD_CODE(0x48, 0x63, 0xf8)  // movsx %eax, %rdi

void compile(const char* source) {
  int x = atoi(source);
  MOV_I32_EAX(x);

  // Ending.
  MOVSX_EAX_RDI();
  SYSCALL(SYSCALL_EXIT);
}

void output_code(FILE* fp) {
  fwrite(code, codesize, 1, fp);
}

////////////////////////////////////////////////

void out_elf_header(FILE* fp, uintptr_t entry) {
  Elf64_Ehdr ehdr = {
    .e_ident     = { ELFMAG0, ELFMAG1, ELFMAG2 ,ELFMAG3,
                     ELFCLASS64, ELFDATA2LSB, EV_CURRENT, ELFOSABI_SYSV },
    .e_type      = ET_EXEC,
    .e_machine   = EM_X86_64,
    .e_version   = EV_CURRENT,
    .e_entry     = entry,
    .e_phoff     = sizeof(Elf64_Ehdr),
    .e_shoff     = 0, // dummy
    .e_flags     = 0x0,
    .e_ehsize    = sizeof(Elf64_Ehdr),
    .e_phentsize = sizeof(Elf64_Phdr),
    .e_phnum     = 1,
    .e_shentsize = 0, // dummy
    .e_shnum     = 0,
    .e_shstrndx  = 0, // dummy
  };

  fwrite(&ehdr, sizeof(Elf64_Ehdr), 1, fp);
}

void out_program_header(FILE* fp, uintptr_t offset, uintptr_t vaddr, uintptr_t filesz, uintptr_t memsz) {
  Elf64_Phdr phdr = {
    .p_type   = PT_LOAD,
    .p_offset = offset,
    .p_vaddr  = vaddr,
    .p_paddr  = 0, // dummy
    .p_filesz = filesz,
    .p_memsz  = memsz,
    .p_flags  = PF_R | PF_X,
    .p_align  = 0x10,
  };

  fwrite(&phdr, sizeof(Elf64_Phdr), 1, fp);
}

int main(int argc, char* argv[]) {
  if (argc < 2) {
    fprintf(stderr, "argc < 2\n");
    return 1;
  }

  compile(argv[1]);

  FILE* fp = stdout;

  out_elf_header(fp, LOAD_ADDRESS);
  out_program_header(fp, PROG_START, LOAD_ADDRESS, codesize, codesize);

  {
    char buf[PROG_START];
    memset(buf, 0, PROG_START);
    fwrite(buf, PROG_START - (sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr)), 1, fp);
  }

  output_code(fp);

  return 0;
}
