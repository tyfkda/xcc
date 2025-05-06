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

    int storage;
    Token *ident;
    const Type *type = parse_var_def(NULL, &storage, &ident);

    FILE *fp = stdout;
    print_type(fp, type);
    if (ident != NULL) {
      fprintf(fp, " %.*s", NAMES(ident->ident));
    }
    fputs("\n", fp);
  }

  return 0;
}
