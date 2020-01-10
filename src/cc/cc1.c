#include <assert.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "codegen.h"
#include "emit.h"
#include "lexer.h"
#include "parser.h"
#include "sema.h"
#include "type.h"
#include "util.h"
#include "var.h"

#include "ir.h"

////////////////////////////////////////////////

#if !defined(SELF_HOSTING)
static void do_dump_ir(Vector *toplevel) {
  for (int i = 0, len = toplevel->len; i < len; ++i) {
    Declaration *decl = toplevel->data[i];
    if (decl == NULL)
      continue;

    switch (decl->kind) {
    case DCL_DEFUN:
      dump_func_ir(decl->defun->func);
      break;
    case DCL_VARDECL:
      break;

    default:
      assert(false);
      break;
    }
  }
}
#endif

////////////////////////////////////////////////

static void init_compiler(FILE *ofp) {
  init_lexer();
  init_emit(ofp);
  gvar_names = new_vector();
}

static Vector *compile1(FILE *ifp, const char *filename, Vector *toplevel) {
  set_source_file(ifp, filename);
  return parse(toplevel);
}

static void compile2(Vector *toplevel) {
  sema(toplevel);
  gen(toplevel);
}

static const char LOCAL_LABEL_PREFIX[] = "--local-label-prefix=";

int main(int argc, char* argv[]) {
  int iarg;
  bool dump_ir = false;

  for (iarg = 1; iarg < argc; ++iarg) {
    if (*argv[iarg] != '-')
      break;
    if (strncmp(argv[iarg], LOCAL_LABEL_PREFIX, sizeof(LOCAL_LABEL_PREFIX) - 1) == 0) {
      set_local_label_prefix(&argv[iarg][sizeof(LOCAL_LABEL_PREFIX) - 1]);
    }
    if (strcmp(argv[iarg], "--dump-ir") == 0)
      dump_ir = true;
  }

  // Compile.
  init_compiler(stdout);

  Vector *toplevel = NULL;
  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      const char *filename = argv[i];
      FILE *ifp = fopen(filename, "r");
      if (ifp == NULL)
        error("Cannot open file: %s\n", filename);
      toplevel = compile1(ifp, filename, toplevel);
    }
  } else {
    toplevel = compile1(stdin, "*stdin*", toplevel);
  }
  compile2(toplevel);

  if (!dump_ir) {
    emit_code(toplevel);
  } else {
#if !defined(SELF_HOSTING)
    do_dump_ir(toplevel);
#endif
  }

  return 0;
}
