#include "../config.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>  // qsort
#include <string.h>
#include <unistd.h>  // isatty

#include "ir_asm.h"
#include "parse_asm.h"
#include "table.h"
#include "util.h"

#define PROG_START      (0x100)
#define START_ADDRESS   (0x01000000 + PROG_START)
#define LOAD_ADDRESS    START_ADDRESS
#define DATA_ALIGN      (0x1000)

static void parse_file(FILE *fp, ParseInfo *info) {
  info->lineno = 1;
  info->rawline = info->p = NULL;
  info->prefetched = NULL;
  set_current_section(info, kSecText, kSegText, SF_EXECUTABLE);

  for (;; ++info->lineno) {
    char *rawline = NULL;
    size_t capa = 0;
    ssize_t len = getline_chomp(&rawline, &capa, fp);
    if (len == -1)
      break;
    info->rawline = rawline;

    SectionInfo *section = info->current_section;
    Vector *irs = section->irs;
    Line line;
    if (!parse_line(&line, info))
      continue;

    if (line.label != NULL) {
      vec_push(irs, new_ir_label(line.label));

      if (!add_label_table(info->label_table, line.label, section, true, false))
        ++info->error_count;
    }

    if (line.dir != NODIRECTIVE)
      handle_directive(info, line.dir);

    if (line.inst != NULL) {
      Code code;
      assemble_inst(line.inst, info, &code);
      if (code.len > 0)
        vec_push(irs, new_ir_code(&code));
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
    label_info = add_label_table(label_table, label, NULL, false, true);
  }
  label_info->flag |= LF_REFERRED;
  return label_info;
}

static void fix_section_size(Vector *sections, uintptr_t start_address) {
  uintptr_t addr = start_address;
  for (int i = 0; i < sections->len; ++i) {
    SectionInfo *section = sections->data[i];
    if ((section->flag & SF_BSS ? section->bss_size : section->ds->len) <= 0)
      continue;
    section->start_address = addr = ALIGN(addr, section->align);
    addr += (section->flag & SF_BSS) ? section->bss_size : section->ds->len;
  }
}

// ================================================

static inline int section_key(const SectionInfo *p) {
  int flag = p->flag;
  // if (flag & SF_BSS)
  //   return 4;
  // if (flag & SF_EXECUTABLE)
  //   return 1;
  // if (flag & SF_WRITABLE)
  //   return 3;
  // return 2;

  // .text(0), .rodata(1), .data(2), .bss(6)
  return (flag & (SF_BSS | SF_WRITABLE | SF_EXECUTABLE)) ^ SF_EXECUTABLE;
}

static int cmp_section(const void *pa, const void *pb) {
  const SectionInfo *sa = *(const SectionInfo**)pa;
  int ka = section_key(sa);
  const SectionInfo *sb = *(const SectionInfo**)pb;
  int kb = section_key(sb);
  int d = ka - kb;
  if (d != 0)
    return d;
  return pa < pb ? -1 : 1;
}

static Vector *sort_sections(Table *section_infos) {
  Vector *sections = new_vector();
  const Name *name;
  SectionInfo *section;
  for (int it = 0; (it = table_iterate(section_infos, it, &name, (void**)&section)) != -1; )
    vec_push(sections, section);
  qsort(sections->data, sections->len, sizeof(void*), cmp_section);
  return sections;
}

static void usage(FILE *fp) {
  fprintf(
      fp,
      "Usage: as [options] file...\n"
      "Options:\n"
      "  -o <filename>       Set output filename (Default: Standard output)\n"
  );
}

int main(int argc, char *argv[]) {
  enum {
    OPT_HELP = 128,
    OPT_VERSION,
  };
  static const struct option options[] = {
    {"o", required_argument},  // Specify output filename
    {"-help", no_argument, OPT_HELP},
    {"v", no_argument, OPT_VERSION},
    {"-version", no_argument, OPT_VERSION},
    {NULL},
  };

  const char *ofn = NULL;
  int opt;
  while ((opt = optparse(argc, argv, options)) != -1) {
    switch (opt) {
    default: assert(false); break;
    case OPT_HELP:
      usage(stdout);
      exit(0);
    case OPT_VERSION:
      show_version("as", XCC_TARGET_ARCH);
      return 0;
    case 'o':
      ofn = optarg;
      break;
    case '?':
      fprintf(stderr, "Warning: unknown option: %s\n", argv[optind - 1]);
      break;
    }
  }
  int iarg = optind;

  // ================================================
  // Run own assembler

  Table section_infos;
  table_init(&section_infos);
  Table label_table;
  table_init(&label_table);

  ParseInfo info;
  info.error_count = 0;
  info.section_infos = &section_infos;
  info.label_table = &label_table;

  if (iarg >= argc)
    error("No input files");

  for (int i = iarg; i < argc; ++i) {
    const char *filename = argv[i];
    FILE *fp;
    if (strcmp(filename, "-") == 0) {
      fp = stdin;
    } else if (!is_file(filename) || (fp = fopen(filename, "r")) == NULL) {
      error("Cannot open %s\n", argv[i]);
    }

    info.filename = filename;
    parse_file(fp, &info);
    fclose(fp);
    if (info.error_count != 0)
      break;
  }

  if (info.error_count != 0) {
    return 1;
  }

  Vector *sections = sort_sections(&section_infos);
  Vector *unresolved = new_vector();
  bool settle1, settle2;
  do {
    settle1 = calc_label_address(LOAD_ADDRESS, sections, &label_table);
    settle2 = resolve_relative_address(sections, &label_table, unresolved);
  } while (!(settle1 && settle2));

  for (int i = 0; i < unresolved->len; ++i) {
    UnresolvedInfo *u = unresolved->data[i];
    make_label_referred(&label_table, u->label, true);
  }

  emit_irs(sections);

  fix_section_size(sections, LOAD_ADDRESS);

#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
  extern int emit_macho_obj(const char *ofn, Vector *sections, Table *label_table,
                            Vector *unresolved);
  #define EMIT_OBJ emit_macho_obj
#else
  extern int emit_elf_obj(const char *ofn, Vector *sections, Table *label_table,
                          Vector *unresolved);
  #define EMIT_OBJ emit_elf_obj
#endif
  int result = EMIT_OBJ(ofn, sections, &label_table, unresolved);
  if (result != 0) {
    if (ofn == NULL && !isatty(STDIN_FILENO))
      drop_all(stdin);
  }
  return result;
}
