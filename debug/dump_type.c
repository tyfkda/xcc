#include <assert.h>
#include <stdio.h>

#include "ast.h"
#include "lexer.h"
#include "parser.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

void dump_type(FILE *fp, const Type *type) {
  if (type->qualifier & TQ_CONST)
    fprintf(fp, "const ");
  if (type->qualifier & TQ_VOLATILE)
    fprintf(fp, "volatile ");

  switch (type->kind) {
  case TY_VOID:
    fprintf(fp, "void");
    break;
  case TY_FIXNUM:
    switch (type->fixnum.kind) {
    case FX_CHAR:  fprintf(fp, "char"); break;
    case FX_SHORT: fprintf(fp, "short"); break;
    case FX_INT:   fprintf(fp, "int"); break;
    case FX_LONG:  fprintf(fp, "long"); break;
    case FX_LLONG: fprintf(fp, "long long"); break;
    case FX_ENUM:  fprintf(fp, "enum"); break;
    default: assert(false); break;
    }
    break;
#ifndef __NO_FLONUM
  case TY_FLONUM:
    switch (type->flonum.kind) {
    case FL_FLOAT:  fprintf(fp, "float"); break;
    case FL_DOUBLE: fprintf(fp, "double"); break;
    default: assert(false); break;
    }
    break;
#endif
  case TY_PTR:
    fprintf(fp, "Ptr<");
    dump_type(fp, type->pa.ptrof);
    fprintf(fp, ">");
    break;
  case TY_ARRAY:
    fprintf(fp, "Array<");
    if (type->pa.length != -1)
      fprintf(fp, "%zu, ", type->pa.length);
    else
      fprintf(fp, "?, ");
    dump_type(fp, type->pa.ptrof);
    fprintf(fp, ">");
    break;
  case TY_FUNC:
    fprintf(fp, "Func<");
    dump_type(fp, type->func.ret);
    fprintf(fp, ": ");
    if (type->func.param_types == NULL) {
      fprintf(fp, "*any*");
    } else if (type->func.param_types->len == 0) {
      fprintf(fp, "void");
    } else {
      for (int i = 0; i < type->func.param_types->len; ++i) {
        if (i > 0)
          fprintf(fp, ", ");
        const Type *t = type->func.param_types->data[i];
        dump_type(fp, t);
      }
      if (type->func.vaargs) {
        fprintf(fp, ", ...");
      }
    }
    fprintf(fp, ">");
    break;
  default: assert(false); break;
  }
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "dump_type: type...\n");
    return 1;
  }

  init_lexer();

  for (int i = 1; i < argc; ++i) {
    char *source = argv[i];
    set_source_string(source, "*type*", 1);

    int storage;
    Token *ident;
    const Type *type = parse_full_type(&storage, &ident);

    FILE *fp = stdout;
    print_type(fp, type);
    fprintf(fp, " => ");
    if (ident != NULL) {
      fprintf(fp, "%.*s: ", ident->ident->bytes, ident->ident->chars);
    }
    dump_type(fp, type);
    fputs("\n", fp);
  }

  return 0;
}
