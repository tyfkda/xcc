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

#define S_ATTR_PURE_INSTRUCTIONS 0x80000000
#define S_ATTR_SOME_INSTRUCTIONS 0x00000400
