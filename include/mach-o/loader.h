#pragma once

#include <stdint.h>

struct mach_header_64 {
  uint32_t magic;
  int32_t cputype;
  int32_t cpusubtype;
  uint32_t filetype;
  uint32_t ncmds;
  uint32_t sizeofcmds;
  uint32_t flags;
  uint32_t reserved;
};

#define MH_MAGIC_64 0xfeedfacf

#define MH_OBJECT 0x1
#define MH_EXECUTE 0x2

#define MH_SUBSECTIONS_VIA_SYMBOLS 0x2000

struct load_command {
  uint32_t cmd;
  uint32_t cmdsize;
};

#define LC_SYMTAB 0x2
#define	LC_DYSYMTAB 0xb
#define LC_SEGMENT_64 0x19
#define LC_BUILD_VERSION 0x32

struct symtab_command {
  uint32_t cmd;
  uint32_t cmdsize;
  uint32_t symoff;
  uint32_t nsyms;
  uint32_t stroff;
  uint32_t strsize;
};

struct dysymtab_command {
    uint32_t cmd;
    uint32_t cmdsize;
    uint32_t ilocalsym;
    uint32_t nlocalsym;
    uint32_t iextdefsym;
    uint32_t nextdefsym;
    uint32_t iundefsym;
    uint32_t nundefsym;
    uint32_t tocoff;
    uint32_t ntoc;
    uint32_t modtaboff;
    uint32_t nmodtab;
    uint32_t extrefsymoff;
    uint32_t nextrefsyms;
    uint32_t indirectsymoff;
    uint32_t nindirectsyms;
    uint32_t extreloff;
    uint32_t nextrel;
    uint32_t locreloff;
    uint32_t nlocrel;
};

struct segment_command_64 { /* for 64-bit architectures */
  uint32_t cmd;
  uint32_t cmdsize;
  char segname[16];
  uint64_t vmaddr;
  uint64_t vmsize;
  uint64_t fileoff;
  uint64_t filesize;
  int32_t maxprot;
  int32_t initprot;
  uint32_t nsects;
  uint32_t flags;
};

struct build_version_command {
  uint32_t cmd;
  uint32_t cmdsize;
  uint32_t platform;
  uint32_t minos;
  uint32_t sdk;
  uint32_t ntools;
};

#define PLATFORM_MACOS 1

struct section_64 { /* for 64-bit architectures */
  char sectname[16];
  char segname[16];
  uint64_t addr;
  uint64_t size;
  uint32_t offset;
  uint32_t align;
  uint32_t reloff;
  uint32_t nreloc;
  uint32_t flags;
  uint32_t reserved1;
  uint32_t reserved2;
  uint32_t reserved3;
};

#define S_REGULAR                0x0
#define S_ZEROFILL               0x1
#define S_CSTRING_LITERALS       0x2
#define S_MOD_INIT_FUNC_POINTERS 0x9

#define S_ATTR_PURE_INSTRUCTIONS 0x80000000
#define S_ATTR_SOME_INSTRUCTIONS 0x00000400
