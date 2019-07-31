#include "stdarg.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#include "codegen.h"
#include "expr.h"
#include "lexer.h"
#include "parser.h"
#include "sema.h"
#include "type.h"
#include "util.h"
#include "var.h"

////////////////////////////////////////////////

static void init_compiler(FILE *fp) {
  init_gen(fp);
  enum_map = new_map();
  enum_value_map = new_map();
  struct_map = new_map();
  typedef_map = new_map();
  gvar_map = new_map();
}

static void compile(FILE *fp, const char *filename) {
  init_lexer(fp, filename);
  Node *node = parse_program();
  node = sema(node);
  gen(node);
}

static const char LOCAL_LABEL_PREFIX[] = "--local-label-prefix=";

int main(int argc, char* argv[]) {
  int iarg;

  for (iarg = 1; iarg < argc; ++iarg) {
    if (*argv[iarg] != '-')
      break;
    if (strncmp(argv[iarg], LOCAL_LABEL_PREFIX, sizeof(LOCAL_LABEL_PREFIX) - 1) == 0) {
      set_local_label_prefix(&argv[iarg][sizeof(LOCAL_LABEL_PREFIX) - 1]);
    }
  }

  // Compile.
  init_compiler(stdout);

  // Test.
  define_global(new_func_type(&tyVoid, NULL, true), 0, NULL, "__asm");

  compile(stdin, "*stdin*");

  fixup_locations();

  return 0;
}
