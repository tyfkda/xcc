#include "wcc.h"

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "lexer.h"
#include "parser.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "wasm_util.h"

////////////////////////////////////////////////

static void emit_wasm(FILE *ofp, Vector *exports) {
  emit_wasm_header(ofp);

  Vector *types = new_vector();
  DataStorage types_section;
  data_init(&types_section);
  for (int i = 0; i < functions->len; ++i) {
    Function *func = functions->data[i];
    const Type *type = func->type;
    assert(type->kind == TY_FUNC);
    int type_index;
    for (type_index = 0; type_index < types->len; ++type_index) {
      const Type *t = types->data[type_index];
      if (same_type(t, type))
        break;
    }
    if (type_index >= types->len) {
      vec_push(types, type);
      data_push(&types_section, WT_FUNC);  // func
      int param_count = type->func.params != NULL ? type->func.params->len : 0;
      emit_uleb128(&types_section, types_section.len, param_count);  // num params
      for (int i = 0; i < param_count; ++i) {
        data_push(&types_section, WT_I32);  // TODO:
      }
      if (type->func.ret->kind == TY_VOID) {
        data_push(&types_section, 0);  // num results
      } else {
        data_push(&types_section, 1);  // num results
        data_push(&types_section, WT_I32);  // TODO:
      }
    }
    func->ra = (RegAlloc*)(intptr_t)type_index;  // Put type index for function here.
  }
  emit_uleb128(&types_section, 0, types->len);  // num types
  emit_uleb128(&types_section, 0, types_section.len);  // Size

  DataStorage functions_section;
  data_init(&functions_section);
  emit_uleb128(&functions_section, functions_section.len, functions->len);  // num functions
  for (int i = 0; i < functions->len; ++i) {
    Function *func = functions->data[i];
    int type_index = (intptr_t)func->ra;  // Extract type index.
    emit_uleb128(&functions_section, functions_section.len, type_index);  // function i signature index
  }
  emit_uleb128(&functions_section, 0, functions_section.len);  // Size

  DataStorage exports_section;
  data_init(&exports_section);
  emit_uleb128(&exports_section, exports_section.len, exports->len);  // num exports
  for (int i = 0; i < exports->len; ++i) {
    const Name *name = exports->data[i];
    VarInfo *varinfo = scope_find(global_scope, name, NULL);
    if (varinfo == NULL) {
      error("Export: `%.*s' not found", name->bytes, name->chars);
      continue;
    }
    if (varinfo->type->kind != TY_FUNC) {
      error("Export: `%.*s' is not function", name->bytes, name->chars);
      continue;
    }
    if (varinfo->storage & VS_STATIC) {
      error("Export: `%.*s' is not public", name->bytes, name->chars);
      continue;
    }

    int func_index;
    for (func_index = 0; func_index < functions->len; ++func_index) {
      Function *func = functions->data[func_index];
      if (equal_name(name, func->name))
        break;
    }
    assert(func_index < functions->len);

    Function *func = functions->data[i];
    int type_index = (intptr_t)func->ra;

    int name_len = name->bytes;
    emit_uleb128(&exports_section, exports_section.len, name_len);  // string length
    data_append(&exports_section, (const unsigned char*)name->chars, name_len);  // export name
    emit_uleb128(&exports_section, exports_section.len, type_index);  // export kind
    emit_uleb128(&exports_section, exports_section.len, func_index);  // export func index
  }
  emit_uleb128(&exports_section, 0, exports_section.len);  // Size

  DataStorage sections;
  data_init(&sections);
  // Types
  data_push(&sections, SEC_TYPE);  // Section "Type" (1)
  data_concat(&sections, &types_section);

  // Functions
  data_push(&sections, SEC_FUNC);  // Section "Function" (3)
  data_append(&sections, functions_section.buf, functions_section.len);

  // Exports
  data_push(&sections, SEC_EXPORT);  // Section "Export" (7)
  data_append(&sections, exports_section.buf, exports_section.len);

  fwrite(sections.buf, sections.len, 1, ofp);

  DataStorage codesec;
  data_init(&codesec);

  data_push(&codesec, SEC_CODE);  // Section "Code" (10)
  size_t size_pos = codesec.len;
  size_t total_code_size = 0;
  for (int i = 0; i < functions->len; ++i) {
    Function *func = functions->data[i];
    DataStorage *code = (DataStorage*)func->bbcon;
    // function body i.
    total_code_size += code->len;
  }
  // Put num function first, and insert total size after.
  emit_uleb128(&codesec, codesec.len, functions->len);  // num functions
  emit_uleb128(&codesec, size_pos, (codesec.len - size_pos) + total_code_size);  // Size
  fwrite(codesec.buf, codesec.len, 1, ofp);

  for (int i = 0; i < functions->len; ++i) {
    Function *func = functions->data[i];
    DataStorage *code = (DataStorage*)func->bbcon;
    fwrite(code->buf, code->len, 1, ofp);
  }
}

////////////////////////////////////////////////

static void init_compiler(void) {
  functions = new_vector();
  init_lexer();
  init_global();

  set_fixnum_size(FX_CHAR,  1, 1);
  set_fixnum_size(FX_SHORT, 2, 2);
  set_fixnum_size(FX_INT,   4, 4);
  set_fixnum_size(FX_LONG,  4, 4);
  set_fixnum_size(FX_LLONG, 8, 8);
  set_fixnum_size(FX_ENUM,  4, 4);
}

static void compile1(FILE *ifp, const char *filename, Vector *toplevel) {
  set_source_file(ifp, filename);
  parse(toplevel);
}

int main(int argc, char *argv[]) {
  const char *ofn = "a.wasm";
  Vector *exports = NULL;
  int iarg;

  init_compiler();

  for (iarg = 1; iarg < argc; ++iarg) {
    char *arg = argv[iarg];
    if (*arg != '-')
      break;

    if (starts_with(arg, "-o")) {
      ofn = arg + 2;
    } else if (starts_with(arg, "-e") && arg[2] != '\0') {
      exports = new_vector();
      const char *s = arg + 2;
      for (;;) {
        const char *p = strchr(s, ',');
        const Name *name = alloc_name(s, p, false);
        vec_push(exports, name);
        if (p == NULL)
          break;
        s = p + 1;
      }
    } else {
      fprintf(stderr, "Unknown option: %s\n", arg);
      return 1;
    }
  }

  if (exports == NULL) {
    error("no exports (require -e<xxx>)\n");
  }

  // Compile.
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

  gen(toplevel);

  FILE *fp = fopen(ofn, "wb");
  if (fp == NULL) {
    error("Cannot open output file");
  } else {
    emit_wasm(fp, exports);
    fclose(fp);
  }

  return 0;
}
