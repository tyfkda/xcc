#include <assert.h>
#include <stdio.h>

#include "ast.h"
#include "fe_misc.h"
#include "lexer.h"
#include "parser.h"
#include "table.h"
#include "type.h"
#include "var.h"

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "dump_type: type...\n");
    return 1;
  }

  init_lexer();

  Scope *scope = new_scope(global_scope);
  curscope = scope;

  for (int i = 1; i < argc; ++i) {
    char *source = argv[i];
    set_source_string(source, "*type*", 1);

    ParsedTypeInfo tinfo;
    const Type *type = parse_var_def(NULL, &tinfo);

    FILE *fp = stdout;
    print_type(fp, type);
    if (tinfo.ident != NULL) {
      fprintf(fp, " %.*s", NAMES(tinfo.ident->ident));
    }
    fputs("\n", fp);
  }

  return 0;
}
