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

static void drop_all(FILE *fp) {
  for (;;) {
    char buf[4096];
    size_t size = fread(buf, 1, sizeof(buf), fp);
    if (size < sizeof(buf))
      break;
  }
}
#endif  // !AS_USE_CC

// ================================================

int main(int argc, char *argv[]) {
  const char *ofn = "a.out";
  int iarg;

  for (iarg = 1; iarg < argc; ++iarg) {
    char *arg = argv[iarg];
    if (*arg != '-')
      break;

    if (starts_with(arg, "-o")) {
      ofn = strdup_(arg + 2);
    } else if (strcmp(arg, "--version") == 0) {
      show_version("as");
      return 0;
    } else {
      fprintf(stderr, "Unknown option: %s\n", arg);
      return 1;
    }
  }

#if !defined(AS_USE_CC)
  // ================================================
  // Run own assembler

  FILE *fp = fopen(ofn, "wb");
  if (fp == NULL) {
    fprintf(stderr, "Failed to open output file: %s\n", ofn);
    if (!isatty(STDIN_FILENO))
      drop_all(stdin);
    return 1;
  }

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

  section_aligns[SEC_DATA] = DATA_ALIGN;

  if (!err) {
    do {
      calc_label_address(LOAD_ADDRESS, section_irs, &label_table);
    } while (!resolve_relative_address(section_irs, &label_table));
    emit_irs(section_irs, &label_table);
  }

  if (err) {
    if (fp != NULL) {
      fclose(fp);
      remove(ofn);
    }
    return 1;
  }

  fix_section_size(LOAD_ADDRESS);

  size_t codesz, rodatasz, datasz, bsssz;
  uintptr_t codeloadadr, dataloadadr;
  get_section_size(SEC_CODE, &codesz, &codeloadadr);
  get_section_size(SEC_RODATA, &rodatasz, NULL);
  get_section_size(SEC_DATA, &datasz, &dataloadadr);
  get_section_size(SEC_BSS, &bsssz, NULL);

  LabelInfo *entry = table_get(&label_table, alloc_name("_start", NULL, false));
  if (entry == NULL)
    error("Cannot find label: `%s'", "_start");

  int phnum = datasz > 0 || bsssz > 0 ? 2 : 1;

  size_t rodata_align = MAX(section_aligns[SEC_RODATA], 1);
  size_t code_rodata_sz = ALIGN(codesz, rodata_align) + rodatasz;
  out_elf_header(fp, entry->address, phnum);
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

#else  // AS_NOT_SUPPORTED
  // ================================================
  // Run system's cc.

  Vector *cc_args = new_vector();
  vec_push(cc_args, "cc");
  vec_push(cc_args, "-o");
  vec_push(cc_args, ofn);

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
#endif
  return 0;
}
