#include <getopt.h>
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

  struct option longopts[] = {
    {"version", no_argument, NULL, 'V'},
    {0},
  };
  int opt;
  int longindex;
  while ((opt = getopt_long(argc, argv, "VI:D:", longopts, &longindex)) != -1) {
    switch (opt) {
    case 'V':
      show_version("cpp");
      return 0;
    case 'I':
      add_system_inc_path(optarg);
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
