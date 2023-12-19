#include "../config.h"

#include <string.h>

#include "preprocessor.h"
#include "util.h"

int main(int argc, char *argv[]) {
  FILE *ofp = stdout;
  init_preprocessor(ofp);

  // Predefeined macros.
  define_macro("__XCC");
#if defined(__linux__)
  define_macro("__linux__");
#elif defined(__APPLE__)
  define_macro("__APPLE__");
#endif
#if defined(__NO_FLONUM)
  define_macro("__NO_FLONUM");
#endif
#if defined(__NO_BITFIELD)
  define_macro("__NO_BITFIELD");
#endif
#if defined(__NO_VLA)
  define_macro("__NO_VLA");
  define_macro("__STDC_NO_VLA__");
#endif
#if defined(__NO_WCHAR)
  define_macro("__NO_WCHAR");
#endif

  enum {
    OPT_ISYSTEM = 128,
    OPT_IDIRAFTER,
  };

  static const struct option options[] = {
    {"I", required_argument},  // Add include path
    {"isystem", required_argument, OPT_ISYSTEM},  // Add system include path
    {"idirafter", required_argument, OPT_IDIRAFTER},  // Add include path (after)
    {"D", required_argument},  // Define macro
    {"-version", no_argument, 'V'},
    {0},
  };
  int opt;
  while ((opt = optparse(argc, argv, options)) != -1) {
    switch (opt) {
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
