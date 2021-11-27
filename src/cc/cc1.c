#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "codegen.h"
#include "emit.h"
#include "emit_code.h"
#include "lexer.h"
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

  //set_fixnum_size(FX_CHAR,  1, 1);
  //set_fixnum_size(FX_SHORT, 2, 2);
  //set_fixnum_size(FX_INT,   4, 4);
  //set_fixnum_size(FX_LONG,  8, 8);
  //set_fixnum_size(FX_LLONG, 8, 8);
  //set_fixnum_size(FX_ENUM,  4, 4);

  install_builtins();
}

static void compile1(FILE *ifp, const char *filename, Vector *decls) {
  set_source_file(ifp, filename);
  parse(decls);
}

static const char LOCAL_LABEL_PREFIX[] = "--local-label-prefix=";

int main(int argc, char *argv[]) {
  int iarg;

  for (iarg = 1; iarg < argc; ++iarg) {
    char *arg = argv[iarg];
    if (*arg != '-')
      break;

    if (starts_with(arg, LOCAL_LABEL_PREFIX)) {
      set_local_label_prefix(&argv[iarg][sizeof(LOCAL_LABEL_PREFIX) - 1]);
    } else if (strcmp(arg, "--version") == 0) {
      show_version("cc1");
      return 0;
    } else {
      fprintf(stderr, "Unknown option: %s\n", arg);
      return 1;
    }
  }

  // Compile.
  init_compiler(stdout);

  toplevel = new_vector();
  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      const char *filename = argv[i];
      FILE *ifp = fopen(filename, "r");
      if (ifp == NULL)
        error("Cannot open file: %s\n", filename);
      compile1(ifp, filename, toplevel);
      fclose(ifp);
    }
  } else {
    compile1(stdin, "*stdin*", toplevel);
  }
  if (compile_error_count != 0)
    exit(1);

  gen(toplevel);
  emit_code(toplevel);

  return 0;
}
