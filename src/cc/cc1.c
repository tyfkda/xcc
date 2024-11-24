#include "../config.h"

#include <ctype.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "codegen.h"
#include "emit_util.h"
#include "emit_code.h"
#include "fe_misc.h"
#include "lexer.h"
#include "optimize.h"  // opt_flags
#include "parser.h"
#include "type.h"
#include "util.h"
#include "var.h"

////////////////////////////////////////////////

extern void install_builtins(void);

static void init_compiler(FILE *ofp) {
  init_lexer();
  init_global();
  init_emit(ofp);

#if XCC_TARGET_PROGRAMMING_MODEL == XCC_PROGRAMMING_MODEL_LP64
  // Default
#elif XCC_TARGET_PROGRAMMING_MODEL == XCC_PROGRAMMING_MODEL_ILP32
  set_fixnum_size(FX_CHAR,  1, 1);
  set_fixnum_size(FX_SHORT, 2, 2);
  set_fixnum_size(FX_INT,   4, 4);
  set_fixnum_size(FX_LONG,  4, 4);
  set_fixnum_size(FX_LLONG, 8, 8);
  set_fixnum_size(FX_ENUM,  4, 4);
#else
# error "Unsupported programming model"
#endif

  install_builtins();
}

static void compile1(FILE *ifp, const char *filename, Vector *decls) {
  set_source_file(ifp, filename);
  parse(decls);
}

static bool parse_fopt(const char *optarg, bool value) {
  static const struct {
    const char *flag_name;
    off_t flag_offset;
  } kFlagTable[] = {
    {"common", offsetof(CcFlags, common)},
  };

  for (size_t i = 0; i < ARRAY_SIZE(kFlagTable); ++i) {
    if (strcmp(optarg, kFlagTable[i].flag_name) == 0) {
      size_t len = strlen(kFlagTable[i].flag_name);
      if (optarg[len] != '\0')
        continue;
      bool *p = (bool*)((char*)&cc_flags + kFlagTable[i].flag_offset);
      *p = value;
      return true;
    }
  }
  return false;
}

int main(int argc, char *argv[]) {
  enum {
    OPT_FNO = 128,
    OPT_SSA,
  };

  static const struct option options[] = {
    {"-version", no_argument, 'V'},

    {"O", optional_argument},  // Optimization level

    // Sub command
    {"fno-", required_argument, OPT_FNO},
    {"f", required_argument},
    {"W", required_argument},

    // Feature flag.
    {"-apply-ssa", no_argument, OPT_SSA},

    {NULL},
  };
  int opt;
  while ((opt = optparse(argc, argv, options)) != -1) {
    switch (opt) {
    default: assert(false); break;
    case 'V':
      show_version("cc1");
      return 0;

    case 'O':
      if (optarg == NULL) {
        cc_flags.optimize_level = 2;
      } else {
        char c = optarg[0];
        switch (c) {
        case '0':
        case '1':
        case '2':
        case '3':
        case 's':
        case 'z':
          cc_flags.optimize_level = isdigit(c) ? c - '0' : c;
          break;
        default:
          fprintf(stderr, "Warning: unknown optimization level: %c\n", c);
          break;
        }
      }
      break;

    case 'f':
    case OPT_FNO:
      if (!parse_fopt(optarg, opt == 'f')) {
        // Silently ignored.
        // fprintf(stderr, "Warning: unknown option for -f: %s\n", optarg);
      }
      break;

    case 'W':
      if (strcmp(optarg, "error") == 0) {
        cc_flags.warn_as_error = true;
      } else {
        // Silently ignored.
        // fprintf(stderr, "Warning: unknown option for -W: %s\n", optarg);
      }
      break;

    case OPT_SSA:
      {
        extern bool apply_ssa;
        apply_ssa = true;
      }
      break;

    case '?':
      fprintf(stderr, "Warning: unknown option: %s\n", argv[optind - 1]);
      break;
    }
  }

  // Compile.
  init_compiler(stdout);

  Vector *toplevel = new_vector();
  int iarg = optind;
  if (iarg >= argc)
    error("No input files");
  for (int i = iarg; i < argc; ++i) {
    const char *filename = argv[i];
    FILE *ifp;
    if (strcmp(filename, "-") == 0) {
      ifp = stdin;
      filename = "<stdin>";
    } else if (!is_file(filename) || (ifp = fopen(filename, "r")) == NULL) {
      error("Cannot open file: %s\n", filename);
    }
    compile1(ifp, filename, toplevel);
    if (ifp != stdin)
      fclose(ifp);
  }
  if (compile_error_count != 0)
    exit(1);
  if (cc_flags.warn_as_error && compile_warning_count != 0)
    exit(2);

  gen(toplevel);
  emit_code(toplevel);

  return 0;
}
