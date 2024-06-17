#include "../config.h"

#include <assert.h>
#include <stdio.h>
#include <unistd.h>  // isatty

#include "gen_section.h"
#include "ir_asm.h"
#include "parse_asm.h"
#include "table.h"
#include "util.h"

#define PROG_START      (0x100)
#define START_ADDRESS   (0x01000000 + PROG_START)
#define LOAD_ADDRESS    START_ADDRESS
#define DATA_ALIGN      (0x1000)

static void parse_file(FILE *fp, const char *filename, Vector **section_irs, Table *label_table) {
  ParseInfo info;
  info.filename = filename;
  info.lineno = 1;
  info.rawline = info.p = NULL;
  info.prefetched = NULL;
  for (;; ++info.lineno) {
    char *rawline = NULL;
    size_t capa = 0;
    ssize_t len = getline_chomp(&rawline, &capa, fp);
    if (len == -1)
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
      assemble_inst(&line->inst, &info, &code);
      if (code.len > 0)
        vec_push(irs, new_ir_code(&code));
    } else {
      handle_directive(&info, line->dir, section_irs, label_table);
    }
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

static LabelInfo *make_label_referred(Table *label_table, const Name *label, bool und) {
  LabelInfo *label_info = table_get(label_table, label);
  if (label_info == NULL) {
    if (!und)
      return NULL;
    const int SEC_UND = -1;
    label_info = add_label_table(label_table, label, SEC_UND, false, true);
  }
  label_info->flag |= LF_REFERRED;
  return label_info;
}

// ================================================

int main(int argc, char *argv[]) {
  const char *ofn = NULL;
  static const struct option options[] = {
    {"o", required_argument},  // Specify output filename
    {"-version", no_argument, 'V'},
    {NULL},
  };
  int opt;
  while ((opt = optparse(argc, argv, options)) != -1) {
    switch (opt) {
    case 'V':
      show_version("as");
      return 0;
    case 'o':
      ofn = optarg;
      break;
    }
  }
  int iarg = optind;

  // ================================================
  // Run own assembler

  Vector *section_irs[SECTION_COUNT];
  Table label_table;
  table_init(&label_table);
  for (int i = 0; i < SECTION_COUNT; ++i)
    section_irs[i] = new_vector();

  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      const char *filename = argv[i];
      FILE *fp;
      if (!is_file(filename) || (fp = fopen(filename, "r")) == NULL)
        error("Cannot open %s\n", argv[i]);
      parse_file(fp, argv[i], section_irs, &label_table);
      fclose(fp);
      if (err)
        break;
    }
  } else {
    parse_file(stdin, "*stdin*", section_irs, &label_table);
  }

  if (err) {
    return 1;
  }

  Vector *unresolved = new_vector();
  bool settle1, settle2;
  do {
    settle1 = calc_label_address(LOAD_ADDRESS, section_irs, &label_table);
    settle2 = resolve_relative_address(section_irs, &label_table, unresolved);
  } while (!(settle1 && settle2));

  for (int i = 0; i < unresolved->len; ++i) {
    UnresolvedInfo *u = unresolved->data[i];
    make_label_referred(&label_table, u->label, true);
  }

  emit_irs(section_irs);

  fix_section_size(LOAD_ADDRESS);

  extern int emit_elf_obj(const char *ofn, Table *label_table, Vector *unresolved);
  int result = emit_elf_obj(ofn, &label_table, unresolved);
  if (result != 0) {
    if (ofn == NULL && !isatty(STDIN_FILENO))
      drop_all(stdin);
  }
  return result;
}
