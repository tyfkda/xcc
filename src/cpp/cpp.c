#include <string.h>

#include "preprocessor.h"
#include "util.h"

int main(int argc, char *argv[]) {
  FILE *ofp = stdout;
  init_preprocessor(ofp);

  // Predefeined macros.
  define_macro_simple("__XCC");
#if defined(__XV6)
  define_macro_simple("__XV6");
#elif defined(__linux__)
  define_macro_simple("__linux__");
#elif defined(__APPLE__)
  define_macro_simple("__APPLE__");
#endif
#if defined(__NO_FLONUM)
  define_macro_simple("__NO_FLONUM");
#endif

  int iarg = 1;
  for (; iarg < argc; ++iarg) {
    char *arg = argv[iarg];
    if (*arg != '-')
      break;

    if (starts_with(arg, "-I")) {
      add_system_inc_path(argv[iarg] + 2);
    } else if (starts_with(argv[iarg], "-D")) {
      define_macro(argv[iarg] + 2);
    } else if (strcmp(arg, "--version") == 0) {
      show_version("cpp");
      return 0;
    } else {
      fprintf(stderr, "Unknown option: %s\n", arg);
      return 1;
    }
  }

  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      const char *filename = argv[i];
      FILE *fp = fopen(filename, "r");
      if (fp == NULL)
        error("Cannot open file: %s\n", filename);
      fprintf(ofp, "# 1 \"%s\" 1\n", filename);
      preprocess(fp, filename);
      fclose(fp);
    }
  } else {
    preprocess(stdin, "*stdin*");
  }
  return 0;
}
