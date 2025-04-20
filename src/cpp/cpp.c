#include "../config.h"

#include <assert.h>
#include <string.h>

#include "preprocessor.h"
#include "util.h"

int main(int argc, char *argv[]) {
  FILE *ofp = stdout;
  init_preprocessor(ofp);

  enum {
    OPT_ISYSTEM = 128,
    OPT_IDIRAFTER,
  };

  static const struct option options[] = {
    {"I", required_argument},  // Add include path
    {"isystem", required_argument, OPT_ISYSTEM},  // Add system include path
    {"idirafter", required_argument, OPT_IDIRAFTER},  // Add include path (after)
    {"D", required_argument},  // Define macro
    {"C", no_argument},  // Do not discard comments
    {"-version", no_argument, 'V'},
    {0},
  };
  int opt;
  while ((opt = optparse(argc, argv, options)) != -1) {
    switch (opt) {
    default: assert(false); break;
    case 'V':
      show_version("cpp");
      return 0;
    case 'I':
      add_inc_path(INC_NORMAL, optarg);
      break;
    case OPT_ISYSTEM:
      add_inc_path(INC_SYSTEM, optarg);
      break;
    case OPT_IDIRAFTER:
      add_inc_path(INC_AFTER, optarg);
      break;
    case 'D':
      define_macro(optarg);
      break;
    case 'C':
      set_preserve_comment(true);
      break;
    case '?':
      fprintf(stderr, "Warning: unknown option: %s\n", argv[optind - 1]);
      break;
    }
  }

  int iarg = optind;
  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      const char *filename = argv[i];
      FILE *fp;
      if (!is_file(filename) || (fp = fopen(filename, "r")) == NULL)
        error("Cannot open file: %s\n", filename);
      preprocess(fp, filename);
      fclose(fp);
    }
  } else {
    preprocess(stdin, "*stdin*");
  }
  return 0;
}
