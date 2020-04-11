// Format of an ELF executable file
#ifndef __ELF_H__
#define __ELF_H__

typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned int uint32;
typedef unsigned long uint64;

typedef uintptr_t uintp;


#define ELFMAG0  (0x7f)
#define ELFMAG1  ('E')
#define ELFMAG2  ('L')
#define ELFMAG3  ('F')

// File header
struct elfhdr {
  uchar e_ident[16];
  ushort e_type;
  ushort e_machine;
  uint e_version;
  uintp e_entry;
  uintp e_phoff;
  uintp e_shoff;
  uint e_flags;
  ushort e_ehsize;
  ushort e_phentsize;
  ushort e_phnum;
  ushort e_shentsize;
  ushort e_shnum;
  ushort e_shstrndx;
};

// Program section header
struct proghdr {
  uint32 p_type;
  uint32 p_flags;
  uint64 p_offset;
  uint64 p_vaddr;
  uint64 p_paddr;
  uint64 p_filesz;
  uint64 p_memsz;
  uint64 p_align;
};

#define ELFCLASS32  (1)
#define ELFCLASS64  (2)

#define ELFDATA2LSB  (1)
// Version
#define EV_CURRENT  (1)

#define ELFOSABI_SYSV  (0)

// Machine
#define EM_386     (3)   // Intel 80386
#define EM_X86_64  (62)  // AMD x86-64 architecture

#define ET_EXEC  (2)  // Executable file

// Values for Proghdr type
#define PT_LOAD  (1)

// Flag bits for Proghdr flags
#define PF_X  (1 << 0)
#define PF_W  (1 << 1)
#define PF_R  (1 << 2)

typedef struct elfhdr Elf64_Ehdr;
typedef struct proghdr Elf64_Phdr;

typedef uint16_t Elf64_Half;
typedef uint32_t Elf64_Word;
typedef uint64_t Elf64_Xword;
typedef int64_t Elf64_Sxword;
typedef uintptr_t Elf64_Addr;
typedef uintptr_t Elf64_Off;

#define SHT_NULL          (0)
#define SHT_PROGBITS      (1)
#define SHT_SYMTAB        (2)
#define SHT_STRTAB        (3)
#define SHT_RELA          (4)
#define SHT_NOBITS        (8)

#define SHF_WRITE         (1 << 0)        /* Writable */
#define SHF_ALLOC         (1 << 1)        /* Occupies memory during execution */
#define SHF_EXECINSTR     (1 << 2)        /* Executable */
#define SHF_INFO_LINK     (1 << 6)        /* `sh_info' contains SHT index */

typedef struct {
  Elf64_Word    sh_name;                /* Section name (string tbl index) */
  Elf64_Word    sh_type;                /* Section type */
  Elf64_Xword   sh_flags;               /* Section flags */
  Elf64_Addr    sh_addr;                /* Section virtual addr at execution */
  Elf64_Off     sh_offset;              /* Section file offset */
  Elf64_Xword   sh_size;                /* Section size in bytes */
  Elf64_Word    sh_link;                /* Link to another section */
  Elf64_Word    sh_info;                /* Additional section information */
  Elf64_Xword   sh_addralign;           /* Section alignment */
  Elf64_Xword   sh_entsize;             /* Entry size if section holds table */
} Elf64_Shdr;

#endif
