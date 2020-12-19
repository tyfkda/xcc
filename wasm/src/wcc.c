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

static const char IMPORT_MODULE_NAME[] = "c";

bool verbose;

////////////////////////////////////////////////

static void emit_wasm(FILE *ofp, Vector *exports) {
  emit_wasm_header(ofp);

  // Types.
  Vector *types = new_vector();
  DataStorage types_section;
  data_init(&types_section);
  {
    const Name *name;
    FuncInfo *info;
    for (int it = 0; (it = table_iterate(&func_info_table, it, &name, (void**)&info)) != -1; ) {
      if (info->flag == 0)
        continue;
      const Type *type = info->type;
      assert(type != NULL && type->kind == TY_FUNC);
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
      info->type_index = type_index;
    }
  }
  emit_uleb128(&types_section, 0, types->len);  // num types
  emit_uleb128(&types_section, 0, types_section.len);  // Size

  // Imports.
  DataStorage imports_section;
  data_init(&imports_section);
  uint32_t imports_count = 0;
  {
    const char *module_name = IMPORT_MODULE_NAME;
    size_t module_name_len = strlen(module_name);

    const Name *name;
    FuncInfo *info;
    for (int it = 0; (it = table_iterate(&func_info_table, it, &name, (void**)&info)) != -1; ) {
      if (info->flag == 0 || info->func != NULL)
        continue;
      const Type *type = info->type;
      assert(type != NULL && type->kind == TY_FUNC);

      VarInfo *varinfo = scope_find(global_scope, name, NULL);
      if (varinfo == NULL) {
        error("Import: `%.*s' not found", name->bytes, name->chars);
        continue;
      }
      if (varinfo->type->kind != TY_FUNC) {
        error("Import: `%.*s' is not function", name->bytes, name->chars);
        continue;
      }
      if (varinfo->storage & VS_STATIC) {
        error("Import: `%.*s' is not public", name->bytes, name->chars);
        continue;
      }

      FuncInfo *info = table_get(&func_info_table, name);
      assert(info != NULL && info->func == NULL);

      uint32_t type_index = info->type_index;

      emit_uleb128(&imports_section, imports_section.len, module_name_len);  // string length
      data_append(&imports_section, (const unsigned char*)module_name, module_name_len);  // import module name
      int name_len = name->bytes;
      emit_uleb128(&imports_section, imports_section.len, name_len);  // string length
      data_append(&imports_section, (const unsigned char*)name->chars, name_len);  // import name
      emit_uleb128(&imports_section, imports_section.len, 0);  // import kind (function)
      emit_uleb128(&imports_section, imports_section.len, type_index);  // import signature index
      ++imports_count;
    }
  }
  if (imports_count > 0) {
    emit_uleb128(&imports_section, 0, imports_count);  // num imports
    emit_uleb128(&imports_section, 0, imports_section.len);  // Size
  }

  // Functions.
  DataStorage functions_section;
  data_init(&functions_section);
  uint32_t function_count = 0;
  {
    const Name *name;
    FuncInfo *info;
    for (int it = 0; (it = table_iterate(&func_info_table, it, &name, (void**)&info)) != -1; ) {
      Function *func = info->func;
      if (func == NULL || info->flag == 0)
        continue;
      ++function_count;
      int type_index = info->type_index;
      emit_uleb128(&functions_section, functions_section.len, type_index);  // function i signature index
    }
  }
  emit_uleb128(&functions_section, 0, function_count);  // num functions
  emit_uleb128(&functions_section, 0, functions_section.len);  // Size

  // Exports.
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

    FuncInfo *info = table_get(&func_info_table, name);
    assert(info != NULL && info->func != NULL);

    uint32_t func_index = info->index;

    int name_len = name->bytes;
    emit_uleb128(&exports_section, exports_section.len, name_len);  // string length
    data_append(&exports_section, (const unsigned char*)name->chars, name_len);  // export name
    emit_uleb128(&exports_section, exports_section.len, 0);  // export kind
    emit_uleb128(&exports_section, exports_section.len, func_index);  // export func index
  }
  emit_uleb128(&exports_section, 0, exports_section.len);  // Size

  // Combine all sections.
  DataStorage sections;
  data_init(&sections);
  // Types
  data_push(&sections, SEC_TYPE);  // Section "Type" (1)
  data_concat(&sections, &types_section);

  // Imports
  if (imports_count > 0) {
    data_push(&sections, SEC_IMPORT);  // Section "Import" (2)
    data_append(&sections, imports_section.buf, imports_section.len);
  }

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
  {
    const Name *name;
    FuncInfo *info;
    for (int it = 0; (it = table_iterate(&func_info_table, it, &name, (void**)&info)) != -1; ) {
      Function *func = info->func;
      if (func == NULL || info->flag == 0)
        continue;
      DataStorage *code = (DataStorage*)func->bbcon;
      // function body i.
      total_code_size += code->len;
    }
  }

  // Put num function first, and insert total size after.
  emit_uleb128(&codesec, codesec.len, function_count);  // num functions
  emit_uleb128(&codesec, size_pos, (codesec.len - size_pos) + total_code_size);  // Size
  fwrite(codesec.buf, codesec.len, 1, ofp);

  {
    const Name *name;
    FuncInfo *info;
    for (int it = 0; (it = table_iterate(&func_info_table, it, &name, (void**)&info)) != -1; ) {
      Function *func = info->func;
      if (func == NULL || info->flag == 0)
        continue;
      DataStorage *code = (DataStorage*)func->bbcon;
      fwrite(code->buf, code->len, 1, ofp);
    }
  }
}

////////////////////////////////////////////////

static void init_compiler(void) {
  table_init(&func_info_table);
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
    } else if (strcmp(arg, "--verbose") == 0) {
      verbose = true;
    } else {
      fprintf(stderr, "Unknown option: %s\n", arg);
      return 1;
    }
  }

  if (exports == NULL) {
    error("no exports (require -e<xxx>)\n");
  }

  VERBOSES("### Exports\n");
  for (int i = 0; i < exports->len; ++i) {
    const Name *name = exports->data[i];
    VERBOSE("%.*s\n", name->bytes, name->chars);
  }
  VERBOSES("\n");

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

  traverse_ast(toplevel, exports);

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
