#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

static void do_dump_ir(Node *node) {
#if defined(SELF_HOSTING)
  UNUSED(node);
#else
  switch (node->kind) {
  case ND_TOPLEVEL:
    for (int i = 0, len = node->toplevel.nodes->len; i < len; ++i) {
      Node *child = node->toplevel.nodes->data[i];
      if (child == NULL)
        continue;
      do_dump_ir(child);
    }
    break;
  case ND_DEFUN:
    {
      Defun *defun = node->defun;
      BBContainer *bbcon = defun->func->bbcon;
      if (bbcon == NULL)
        break;
      fprintf(stderr, "%s: // BB: #%d\n", defun->func->name, bbcon->bbs->len);
      for (int i = 0; i < bbcon->bbs->len; ++i) {
        BB *bb = bbcon->bbs->data[i];
        fprintf(stderr, "// BB %d\n", i);
        fprintf(stderr, "%s:\n", bb->label);
        for (int j = 0; j < bb->irs->len; ++j) {
          IR *ir = bb->irs->data[j];
          dump_ir(ir);
        }
      }
      fprintf(stderr, "\n");
    }
    break;

  default:
    break;
  }
#endif
}

////////////////////////////////////////////////

static void init_compiler(FILE *ofp) {
  init_emit(ofp);
  enum_map = new_map();
  enum_value_map = new_map();
  struct_map = new_map();
  typedef_map = new_map();
  gvar_map = new_map();
}

static Vector *compile1(FILE *ifp, const char *filename, Vector *nodes) {
  init_lexer(ifp, filename);
  return parse_program(nodes);
}

static Node *compile2(Vector *nodes) {
  Node *top = sema(new_top_node(nodes));
  gen(top);
  emit_code(top);
  return top;
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

  Vector *nodes = NULL;
  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      const char *filename = argv[i];
      FILE *ifp = fopen(filename, "r");
      if (ifp == NULL)
        error("Cannot open file: %s\n", filename);
      nodes = compile1(ifp, filename, nodes);
    }
  } else {
    nodes = compile1(stdin, "*stdin*", nodes);
  }
  Node *root = compile2(nodes);

  if (dump_ir) {
    do_dump_ir(root);
  }

  return 0;
}
