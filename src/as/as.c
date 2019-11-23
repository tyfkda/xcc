#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdint.h>  // uintptr_t
#include <stdio.h>
#include <stdlib.h>  // malloc, calloc
#include <string.h>
#include <strings.h>  // strncasecmp

#include "asm_x86.h"
#include "elfutil.h"
#include "gen.h"
#include "ir_asm.h"
#include "parse_asm.h"
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

#if !defined(AS_USE_CC)
void parse_file(FILE *fp, Vector **section_irs, Map *label_map) {
  for (;;) {
    char *rawline = NULL;
    size_t capa = 0;
    ssize_t len = getline_(&rawline, &capa, fp, 0);
    if (len == EOF)
      break;

    Vector *irs = section_irs[current_section];
    Line *line = parse_line(rawline);

    if (line->label != NULL) {
      vec_push(irs, new_ir_label(line->label));

      void *address;
      if (map_try_get(label_map, line->label, &address)) {
        fprintf(stderr, "`%s' already defined\n", line->label);
        err = true;
      }
      map_put(label_map, line->label, NULL);
    }

    if (line->dir == NODIRECTIVE) {
      Code code;
      assemble_inst(&line->inst, line->rawline, &code);
      if (code.len > 0)
        vec_push(irs, new_ir_code(&code));
    } else {
      handle_directive(line->dir, line->directive_line, section_irs);
    }
  }
}

static void put_padding(FILE* fp, uintptr_t start) {
  long cur = ftell(fp);
   if (start > (size_t)cur) {
    size_t size = start - (uintptr_t)cur;
    char* buf = calloc(1, size);
    fwrite(buf, size, 1, fp);
    free(buf);
  }
}
#endif  // !AS_USE_CC

// ================================================

int main(int argc, char* argv[]) {
  const char *ofn = "a.out";
  int iarg;

  for (iarg = 1; iarg < argc; ++iarg) {
    if (*argv[iarg] != '-')
      break;
    if (strncmp(argv[iarg], "-o", 2) == 0)
      ofn = strdup_(argv[iarg] + 2);
  }

#if !defined(AS_USE_CC)
  // ================================================
  // Run own assembler

  FILE* fp = fopen(ofn, "wb");
  if (fp == NULL) {
    fprintf(stderr, "Failed to open output file: %s\n", ofn);
    return 1;
  }

  Vector *section_irs[SECTION_COUNT];
  Map *label_map = new_map();
  for (int i = 0; i < SECTION_COUNT; ++i)
    section_irs[i] = new_vector();

  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      FILE *fp = fopen(argv[i], "r");
      if (fp == NULL)
        error("Cannot open %s\n", argv[i]);
      parse_file(fp, section_irs, label_map);
      fclose(fp);
      if (err)
        break;
    }
  } else {
    parse_file(stdin, section_irs, label_map);
  }

  if (!err) {
    do {
      calc_label_address(LOAD_ADDRESS, section_irs, label_map);
    } while (!resolve_relative_address(section_irs, label_map));
    emit_irs(section_irs, label_map);
  }

  if (err) {
    if (fp != NULL) {
      fclose(fp);
      remove(ofn);
    }
    return 1;
  }

  fix_section_size(LOAD_ADDRESS);

  size_t codefilesz, codememsz;
  size_t datafilesz, datamemsz;
  uintptr_t codeloadadr, dataloadadr;
  get_section_size(SEC_CODE, &codefilesz, &codememsz, &codeloadadr);
  get_section_size(SEC_DATA, &datafilesz, &datamemsz, &dataloadadr);

  void *entry;
  if (!map_try_get(label_map, "_start", &entry))
    error("Cannot find label: `%s'", "_start");

  int phnum = datamemsz > 0 ? 2 : 1;

  out_elf_header(fp, (uintptr_t)entry, phnum);
  out_program_header(fp, 0, PROG_START, codeloadadr, codefilesz, codememsz);
  if (phnum > 1)
    out_program_header(fp, 1, ALIGN(PROG_START + codefilesz, 0x1000), dataloadadr, datafilesz, datamemsz);

  put_padding(fp, PROG_START);
  output_section(fp, SEC_CODE);
  if (phnum > 1) {
    put_padding(fp, ALIGN(PROG_START + codefilesz, 0x1000));
    output_section(fp, SEC_DATA);
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
