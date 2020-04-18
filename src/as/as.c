#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdint.h>  // uintptr_t
#include <stdio.h>
#include <stdlib.h>  // malloc, calloc
#include <string.h>
#include <strings.h>  // strncasecmp
#include <unistd.h>

#include "asm_x86.h"
#include "elfutil.h"
#include "gen.h"
#include "ir_asm.h"
#include "parse_asm.h"
#include "table.h"
#include "util.h"

#define PROG_START   (0x100)

#if defined(__XV6)
// XV6
#include "../kernel/syscall.h"
#include "../kernel/traps.h"

#define START_ADDRESS    0x1000

#elif defined(__linux__)
// Linux

#include <sys/stat.h>

#define START_ADDRESS    (0x01000000 + PROG_START)

#else

#include <unistd.h>
#define AS_USE_CC

#endif

#define LOAD_ADDRESS    START_ADDRESS
#define DATA_ALIGN      (0x1000)

#if !defined(AS_USE_CC)
void parse_file(FILE *fp, const char *filename, Vector **section_irs, Table *label_table) {
  ParseInfo info;
  info.filename = filename;
  info.lineno = 1;
  for (;; ++info.lineno) {
    char *rawline = NULL;
    size_t capa = 0;
    ssize_t len = getline_(&rawline, &capa, fp, 0);
    if (len == EOF)
      break;
    info.rawline = rawline;

    Vector *irs = section_irs[current_section];
    Line *line = parse_line(&info);
    if (line == NULL)
      continue;

    if (line->label != NULL) {
      vec_push(irs, new_ir_label(line->label));

      if (!add_label_table(label_table, line->label, current_section, true, false))
        err = true;
    }

    if (line->dir == NODIRECTIVE) {
      Code code;
      if (assemble_inst(&line->inst, &info, &code)) {
        if (code.len > 0)
          vec_push(irs, new_ir_code(&code));
      }
    } else {
      handle_directive(&info, line->dir, section_irs, label_table);
    }
  }
}

static void put_padding(FILE *fp, uintptr_t start) {
  long cur = ftell(fp);
  if (start > (size_t)cur) {
    size_t size = start - (uintptr_t)cur;
    char *buf = calloc(1, size);
    fwrite(buf, size, 1, fp);
    free(buf);
  }
}

static void putnum(FILE *fp, unsigned long num, int bytes) {
  for (int i = 0; i < bytes; ++i) {
    fputc(num, fp);
    num >>= 8;
  }
}

static void drop_all(FILE *fp) {
  for (;;) {
    char buf[4096];
    size_t size = fread(buf, 1, sizeof(buf), fp);
    if (size < sizeof(buf))
      break;
  }
}

static int output_obj(const char *ofn, Table *label_table) {
  UNUSED(label_table);

  size_t codesz, rodatasz, datasz, bsssz;
  get_section_size(SEC_CODE, &codesz, NULL);
  get_section_size(SEC_RODATA, &rodatasz, NULL);
  get_section_size(SEC_DATA, &datasz, NULL);
  get_section_size(SEC_BSS, &bsssz, NULL);

  FILE *ofp;
  if (ofn == NULL) {
    ofp = stdout;
  } else {
    ofp = fopen(ofn, "wb");
    if (ofp == NULL) {
      fprintf(stderr, "Failed to open output file: %s\n", ofn);
      if (!isatty(STDIN_FILENO))
        drop_all(stdin);
      return 1;
    }
  }

  uintptr_t entry = 0;
  int phnum = 0;
  int shnum = 6;
  out_elf_header(ofp, entry, phnum, shnum);

  uintptr_t addr = sizeof(Elf64_Ehdr);
  uintptr_t code_ofs = addr;
  output_section(ofp, SEC_CODE);
  uintptr_t rodata_ofs = addr += codesz;
  if (rodatasz > 0) {
    rodata_ofs = ALIGN(rodata_ofs, 0x10);
    put_padding(ofp, rodata_ofs);
    output_section(ofp, SEC_RODATA);
    addr = rodata_ofs + rodatasz;
  }
  uintptr_t data_ofs = addr;
  if (datasz > 0) {
    data_ofs = ALIGN(data_ofs, 0x10);
    put_padding(ofp, data_ofs);
    output_section(ofp, SEC_DATA);
    addr = data_ofs + datasz;
  }
  uintptr_t bss_ofs = addr;
  if (bsssz > 0) {
    bss_ofs = ALIGN(bss_ofs, 0x10);
    put_padding(ofp, bss_ofs);
    addr = bss_ofs;
  }
  put_padding(ofp, ALIGN(addr, 0x10));

  // Set up shstrtab.
  Strtab shstrtab;
  strtab_init(&shstrtab);

  // Output section headers.
  {
    Elf64_Shdr nulsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name("", NULL, false)),
      .sh_type = SHT_NULL,
      .sh_addralign = 1,
    };
    Elf64_Shdr textsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".text", NULL, false)),
      .sh_type = SHT_PROGBITS,
      .sh_flags = SHF_EXECINSTR | SHF_ALLOC,
      .sh_addr = 0,
      .sh_offset = code_ofs,
      .sh_size = codesz,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = MAX(section_aligns[SEC_CODE], 1),
      .sh_entsize = 0,
    };
    Elf64_Shdr rodatasec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".rodata", NULL, false)),
      .sh_type = SHT_PROGBITS,
      .sh_flags = SHF_ALLOC,
      .sh_addr = 0,
      .sh_offset = rodata_ofs,
      .sh_size = rodatasz,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = MAX(section_aligns[SEC_RODATA], 1),
      .sh_entsize = 0,
    };
    Elf64_Shdr datasec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".data", NULL, false)),
      .sh_type = SHT_PROGBITS,
      .sh_flags = SHF_WRITE | SHF_ALLOC,
      .sh_addr = 0,
      .sh_offset = data_ofs,
      .sh_size = datasz,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = MAX(section_aligns[SEC_DATA], 1),
      .sh_entsize = 0,
    };
    Elf64_Shdr bsssec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".bss", NULL, false)),
      .sh_type = SHT_NOBITS,
      .sh_flags = SHF_WRITE | SHF_ALLOC,
      .sh_addr = 0,
      .sh_offset = bss_ofs,
      .sh_size = bsssz,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = MAX(section_aligns[SEC_BSS], 1),
      .sh_entsize = 0,
    };
    Elf64_Shdr shstrtabsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".shstrtab", NULL, false)),
      .sh_type = SHT_STRTAB,
      .sh_flags = 0,
      .sh_addr = 0,
      .sh_offset = 0,  // Dummy
      .sh_size = 0,    // Dummy
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = 1,
      .sh_entsize = 0,
    };

    long shstrtab_ofs;
    {
      void *buf = strtab_dump(&shstrtab);
      long cur = ftell(ofp);
      shstrtab_ofs = ALIGN(cur, 0x10);
      put_padding(ofp, shstrtab_ofs);
      fwrite(buf, shstrtab.size, 1, ofp);
    }
    shstrtabsec.sh_offset = shstrtab_ofs;
    shstrtabsec.sh_size = shstrtab.size;

    long cur = ftell(ofp);
    long sh_ofs = ALIGN(cur, 0x10);
    put_padding(ofp, sh_ofs);

    fwrite(&nulsec, sizeof(nulsec), 1, ofp);
    fwrite(&textsec, sizeof(textsec), 1, ofp);
    fwrite(&rodatasec, sizeof(rodatasec), 1, ofp);
    fwrite(&datasec, sizeof(datasec), 1, ofp);
    fwrite(&bsssec, sizeof(bsssec), 1, ofp);
    fwrite(&shstrtabsec, sizeof(shstrtabsec), 1, ofp);

    // Write section table offset.
    fseek(ofp, 0x28, SEEK_SET);
    putnum(ofp, sh_ofs, 8);
  }

  return 0;
}

static int output_exe(const char *ofn, Table *label_table) {
  size_t codesz, rodatasz, datasz, bsssz;
  uintptr_t codeloadadr, dataloadadr;
  get_section_size(SEC_CODE, &codesz, &codeloadadr);
  get_section_size(SEC_RODATA, &rodatasz, NULL);
  get_section_size(SEC_DATA, &datasz, &dataloadadr);
  get_section_size(SEC_BSS, &bsssz, NULL);

  LabelInfo *entry = table_get(label_table, alloc_name("_start", NULL, false));
  if (entry == NULL)
    error("Cannot find label: `%s'", "_start");

  int phnum = datasz > 0 || bsssz > 0 ? 2 : 1;

  FILE *fp;
  if (ofn == NULL) {
    fp = stdout;
  } else {
    fp = fopen(ofn, "wb");
    if (fp == NULL) {
      fprintf(stderr, "Failed to open output file: %s\n", ofn);
      if (!isatty(STDIN_FILENO))
        drop_all(stdin);
      return 1;
    }
  }

  size_t rodata_align = MAX(section_aligns[SEC_RODATA], 1);
  size_t code_rodata_sz = ALIGN(codesz, rodata_align) + rodatasz;
  out_elf_header(fp, entry->address, phnum, 0);
  out_program_header(fp, 0, PROG_START, codeloadadr, code_rodata_sz, code_rodata_sz);
  if (phnum > 1) {
    size_t bss_align = MAX(section_aligns[SEC_BSS], 1);
    size_t datamemsz = ALIGN(datasz, bss_align) + bsssz;
    out_program_header(fp, 1, ALIGN(PROG_START + code_rodata_sz, DATA_ALIGN), dataloadadr, datasz, datamemsz);
  }

  uintptr_t addr = PROG_START;
  put_padding(fp, addr);
  output_section(fp, SEC_CODE);
  addr += codesz;
  if (rodatasz > 0) {
    size_t rodata_align = MAX(section_aligns[SEC_RODATA], 1);
    addr = ALIGN(addr, rodata_align);
    put_padding(fp, addr);
    output_section(fp, SEC_RODATA);
    addr += rodatasz;
  }
  if (datasz > 0) {
    addr = ALIGN(addr, DATA_ALIGN);
    put_padding(fp, addr);
    output_section(fp, SEC_DATA);
    addr += datasz;
  }
  fclose(fp);

#if !defined(__XV6) && defined(__linux__)
  if (chmod(ofn, 0755) == -1) {
    perror("chmod failed\n");
    return 1;
  }
#endif
  return 0;
}
#endif  // !AS_USE_CC

// ================================================

int main(int argc, char *argv[]) {
  const char *ofn = NULL;
  bool out_obj = false;
  int iarg;

  for (iarg = 1; iarg < argc; ++iarg) {
    char *arg = argv[iarg];
    if (*arg != '-')
      break;

    if (strcmp(arg, "-c") == 0) {
      out_obj = true;
    } else if (starts_with(arg, "-o")) {
      ofn = strdup_(arg + 2);
    } else if (strcmp(arg, "--version") == 0) {
      show_version("as");
      return 0;
    } else {
      fprintf(stderr, "Unknown option: %s\n", arg);
      return 1;
    }
  }

  if (ofn == NULL) {
    if (out_obj) {
      if (iarg < argc)
        ofn = change_ext(argv[iarg], "o");
    } else {
      ofn = "a.out";
    }
  }

#if !defined(AS_USE_CC)
  // ================================================
  // Run own assembler

  Vector *section_irs[SECTION_COUNT];
  Table label_table;
  table_init(&label_table);
  for (int i = 0; i < SECTION_COUNT; ++i)
    section_irs[i] = new_vector();

  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      FILE *fp = fopen(argv[i], "r");
      if (fp == NULL)
        error("Cannot open %s\n", argv[i]);
      parse_file(fp, argv[i], section_irs, &label_table);
      fclose(fp);
      if (err)
        break;
    }
  } else {
    parse_file(stdin, "*stdin*", section_irs, &label_table);
  }

  if (!out_obj)
    section_aligns[SEC_DATA] = DATA_ALIGN;

  if (!err) {
    do {
      calc_label_address(LOAD_ADDRESS, section_irs, &label_table);
    } while (!resolve_relative_address(section_irs, &label_table));
    emit_irs(section_irs, &label_table);
  }

  if (err) {
    return 1;
  }

  fix_section_size(LOAD_ADDRESS);

  return out_obj ? output_obj(ofn, &label_table) : output_exe(ofn, &label_table);

#else  // AS_NOT_SUPPORTED
  // ================================================
  // Run system's cc.

  Vector *cc_args = new_vector();
  vec_push(cc_args, "cc");
  vec_push(cc_args, "-o");
  vec_push(cc_args, ofn);
  if (out_obj)
    vec_push(cc_args, "-c");

  char temp_file_name[FILENAME_MAX + 2];
  if (iarg >= argc) {
    // Read from stdin and write to temporary file.
    char *tmpdir = getenv("TMPDIR");
    if (tmpdir == NULL)
      tmpdir = "/tmp";

    snprintf(temp_file_name, sizeof(temp_file_name), "%s/as_XXXXXX", tmpdir);
    mkstemp(temp_file_name);
    strcat(temp_file_name, ".s");
    FILE *tmpfp = fopen(temp_file_name, "w");
    if (tmpfp == NULL)
      error("Failed to open temporary file");
    for (;;) {
      static char buf[4096];
      size_t size = fread(buf, 1, sizeof(buf), stdin);
      if (size > 0)
        fwrite(buf, 1, size, tmpfp);
      if (size < sizeof(buf))
        break;
    }
    fclose(tmpfp);

    vec_push(cc_args, temp_file_name);
  } else {
    for (int i = iarg; i < argc; ++i) {
      vec_push(cc_args, argv[i]);
    }
  }

  vec_push(cc_args, NULL);
  if (execvp(cc_args->data[0], (char *const*)cc_args->data) < 0)
    error("Failed to call cc");

  return 0;
#endif
}
