#include "../config.h"
#include "wcc.h"

#include <assert.h>
#include <stdlib.h>  // free, qsort
#include <string.h>

#include "ast.h"
#include "cc_misc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "wasm_util.h"

static void emit_global_number(void *ud, const Type *type, Expr *var, Fixnum offset) {
  DataStorage *ds = ud;

  switch (type->kind) {
  case TY_FIXNUM:
  case TY_PTR:
    {
      Fixnum v = offset;
      if (var != NULL) {
        if (var->type->kind == TY_FUNC) {
          assert(offset == 0);
          v += get_indirect_function_index(var->var.name);
        } else {
          GVarInfo *info = get_gvar_info(var);
          assert(!is_prim_type(info->varinfo->type) || (info->varinfo->storage & VS_REF_TAKEN));
          v += info->non_prim.address;
        }
      }
      data_push(ds, type_size(type) <= I32_SIZE ? OP_I32_CONST : OP_I64_CONST);
      data_leb128(ds, -1, v);
    }
    break;
#ifndef __NO_FLONUM
  case TY_FLONUM:
    {
      assert(var == NULL);
      if (type->flonum.kind < FL_DOUBLE) {
        data_push(ds, OP_F32_CONST);
        uint32_t f = offset;
        data_append(ds, (unsigned char*)&f, sizeof(float));  // TODO: Endian
      } else {
        data_push(ds, OP_F64_CONST);
        uint64_t d = offset;
        data_append(ds, (unsigned char*)&d, sizeof(double));  // TODO: Endian
      }
    }
    break;
#endif
  default: assert(false); break;
  }
}

static void construct_primitive_global(DataStorage *ds, const VarInfo *varinfo) {
  static const ConstructInitialValueVTable kVtable = {
    // Only .emit_number is required.
    .emit_number = emit_global_number,
  };

  const Type *type = varinfo->type;
  Initializer *init = varinfo->global.init;
  construct_initial_value(type, init, &kVtable, ds);
}

static void emit_fixnum(DataStorage *ds, Fixnum v, size_t size) {
  // Assume endian and CHAR_BIT are same on host and target.
  data_append(ds, (unsigned char *)&v, size);
}

static void emit_align(void *ud, int align) {
  DataStorage *ds = ud;
  int offset = ds->len;
  int d = offset % align;
  if (d > 0) {
    d = align - d;
    for (int i = 0; i < d; ++i)
      data_push(ds, 0);
  }
}
static void emit_number(void *ud, const Type *type, Expr *var, Fixnum offset) {
  Fixnum v = offset;
  if (var != NULL) {
    assert(var->kind == EX_VAR);
    if (var->type->kind == TY_FUNC) {
      assert(v == 0);
      v = get_indirect_function_index(var->var.name);
    } else {
      assert(var->kind == EX_VAR);
      const GVarInfo *info = get_gvar_info(var);
      assert(!is_prim_type(info->varinfo->type) || (info->varinfo->storage & VS_REF_TAKEN));
      v += info->non_prim.address;
    }
  }
  DataStorage *ds = ud;
  emit_fixnum(ds, v, type_size(type));
}
static void emit_string(void *ud, Expr *str, size_t size) {
  assert(str->kind == EX_STR);
  DataStorage *ds = ud;
  size_t src_size = str->str.len * type_size(str->type->pa.ptrof);
  if (size > src_size) {
    unsigned char *buf = calloc_or_die(size);
    assert(buf != NULL);
    memcpy(buf, str->str.buf, src_size);
    data_append(ds, buf, size);
    free(buf);
  } else {
    data_append(ds, (unsigned char*)str->str.buf, size);
  }
}

static void construct_data_segment(DataStorage *ds) {
  static const ConstructInitialValueVTable kVtable = {
    .emit_align = emit_align,
    .emit_number = emit_number,
    .emit_string = emit_string,
  };

  // Enumerate global variables.
  const Name *name;
  GVarInfo *info;
  uint32_t address = 0;

  unsigned char *zerobuf = NULL;
  size_t zerosz = 0;

  for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
    const VarInfo *varinfo = info->varinfo;
    if ((is_prim_type(varinfo->type) && !(varinfo->storage & VS_REF_TAKEN)) ||
        varinfo->global.init == NULL)
      continue;
    uint32_t adr = info->non_prim.address;
    assert(adr >= address);
    if (adr > address) {
      uint32_t sz = adr - address;
      if (sz > zerosz) {
        zerobuf = realloc_or_die(zerobuf, sz);
        memset(zerobuf + zerosz, 0x00, sz - zerosz);
        zerosz = sz;
      }
      data_append(ds, zerobuf, sz);
    }

    construct_initial_value(varinfo->type, varinfo->global.init, &kVtable, ds);

    address = adr + type_size(varinfo->type);
    assert(ds->len == address);
  }
}

static int compare_indirect(const void *pa, const void *pb) {
  const FuncInfo *qa = *(const FuncInfo**)pa;
  const FuncInfo *qb = *(const FuncInfo**)pb;
  return (int)qa->indirect_index - (int)qb->indirect_index;
}

void emit_wasm(FILE *ofp, Vector *exports, const char *import_module_name,
               uint32_t address_bottom) {
  emit_wasm_header(ofp);

  // Types.
  DataStorage types_section;
  data_init(&types_section);
  for (int i = 0, len = functypes->len; i < len; ++i) {
    const DataStorage *wt = functypes->data[i];
    data_push(&types_section, WT_FUNC);  // func
    data_append(&types_section, wt->buf, wt->len);
  }
  data_uleb128(&types_section, 0, functypes->len);  // num types
  data_uleb128(&types_section, 0, types_section.len);  // Size

  // Imports.
  DataStorage imports_section;
  data_init(&imports_section);
  uint32_t imports_count = 0;
  {
    const char *module_name = import_module_name;
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
        error("Import: `%.*s' not found", NAMES(name));
      }
      if (varinfo->type->kind != TY_FUNC) {
        error("Import: `%.*s' is not function", NAMES(name));
      }
      if (varinfo->storage & VS_STATIC) {
        error("Import: `%.*s' is not public", NAMES(name));
      }

      FuncInfo *info = table_get(&func_info_table, name);
      assert(info != NULL && info->func == NULL);

      uint32_t type_index = info->type_index;

      data_uleb128(&imports_section, -1, module_name_len);  // string length
      data_append(&imports_section, (const unsigned char*)module_name,
                  module_name_len);  // import module name
      int name_len = name->bytes;
      data_uleb128(&imports_section, -1, name_len);  // string length
      data_append(&imports_section, (const unsigned char*)name->chars, name_len);  // import name
      data_push(&imports_section, IMPORT_FUNC);  // import kind
      data_uleb128(&imports_section, -1, type_index);  // import signature index
      ++imports_count;
    }
  }
  if (imports_count > 0) {
    data_uleb128(&imports_section, 0, imports_count);  // num imports
    data_uleb128(&imports_section, 0, imports_section.len);  // Size
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
      data_uleb128(&functions_section, -1, type_index);  // function i signature index
    }
  }
  data_uleb128(&functions_section, 0, function_count);  // num functions
  data_uleb128(&functions_section, 0, functions_section.len);  // Size

  // Table.
  DataStorage table_section;
  data_init(&table_section);
  // TODO: Output table only when it is needed.
  int table_start_index = 1;
  data_leb128(&table_section, -1, 1);  // num tables
  data_push(&table_section, WT_FUNCREF);
  data_push(&table_section, 0x00);  // limits: flags
  data_leb128(&table_section, -1, table_start_index + indirect_function_table.count);  // initial

  // Memory.
  DataStorage memory_section;
  data_init(&memory_section);
  {
    uint32_t page_count = (address_bottom + MEMORY_PAGE_SIZE - 1) / MEMORY_PAGE_SIZE;
    if (page_count <= 0)
      page_count = 1;
    data_uleb128(&memory_section, -1, 1);  // count?
    data_uleb128(&memory_section, -1, 0);  // index?
    data_uleb128(&memory_section, -1, page_count);
  }

  // Globals.
  DataStorage globals_section;
  data_init(&globals_section);
  uint32_t globals_count = 0;
  {
    const Name *name;
    GVarInfo *info;
    for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
      const VarInfo *varinfo = info->varinfo;
      if (!is_prim_type(varinfo->type) || varinfo->storage & VS_REF_TAKEN)
        continue;
      unsigned char wt = to_wtype(varinfo->type);
      data_push(&globals_section, wt);
      data_push(&globals_section, (varinfo->type->qualifier & TQ_CONST) == 0);  // global mutability
      assert(varinfo->global.init == NULL || varinfo->global.init->kind == IK_SINGLE);
      construct_primitive_global(&globals_section, varinfo);
      data_push(&globals_section, OP_END);
      ++globals_count;
    }
  }
  if (globals_count > 0) {
    data_uleb128(&globals_section, 0, globals_count);  // num globals
    data_uleb128(&globals_section, 0, globals_section.len);  // Size
  }

  // Exports.
  DataStorage exports_section;
  data_init(&exports_section);
  int num_exports = 0;
  for (int i = 0; i < exports->len; ++i) {
    const Name *name = exports->data[i];
    VarInfo *varinfo = scope_find(global_scope, name, NULL);
    if (varinfo == NULL) {
      error("Export: `%.*s' not found", NAMES(name));
    }
    if (varinfo->type->kind != TY_FUNC) {
      error("Export: `%.*s' is not function", NAMES(name));
    }
    if (varinfo->storage & VS_STATIC) {
      error("Export: `%.*s' is not public", NAMES(name));
    }

    FuncInfo *info = table_get(&func_info_table, name);
    assert(info != NULL && info->func != NULL);

    uint32_t func_index = info->index;

    int name_len = name->bytes;
    data_uleb128(&exports_section, -1, name_len);  // string length
    data_append(&exports_section, (const unsigned char*)name->chars, name_len);  // export name
    data_uleb128(&exports_section, -1, IMPORT_FUNC);  // export kind
    data_uleb128(&exports_section, -1, func_index);  // export func index
    ++num_exports;
  }
  // Export globals.
  {
    const Name *name;
    GVarInfo *info;
    for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
      const VarInfo *varinfo = info->varinfo;
      if (!info->is_export || !is_prim_type(varinfo->type) || (varinfo->storage & VS_REF_TAKEN))
        continue;
      data_uleb128(&exports_section, -1, name->bytes);  // string length
      data_append(&exports_section, (const unsigned char*)name->chars, name->bytes);  // export name
      data_push(&exports_section, EXPORT_GLOBAL);  // export kind
      data_uleb128(&exports_section, -1, info->prim.index);  // export global index
      ++num_exports;
    }
  }
  /*if (memory_section.len > 0)*/ {  // TODO: Export only if memory exists
    static const char name[] = "memory";
    data_uleb128(&exports_section, -1, sizeof(name) - 1);  // string length
    data_append(&exports_section, (const unsigned char*)name, sizeof(name) - 1);  // export name
    data_uleb128(&exports_section, -1, IMPORT_MEMORY);  // export kind
    data_uleb128(&exports_section, -1, 0);  // export global index
    ++num_exports;
  }
  data_uleb128(&exports_section, 0, num_exports);  // num exports
  data_uleb128(&exports_section, 0, exports_section.len);  // Size

  // Elements.
  DataStorage elems_section;
  data_init(&elems_section);
  if (indirect_function_table.count > 0) {
    int count = indirect_function_table.count;
    FuncInfo **indirect_funcs = ALLOCA(sizeof(*indirect_funcs) * count);

    // Enumerate imported functions.
    VERBOSES("### Indirect functions\n");
    const Name *name;
    FuncInfo *info;
    int index = 0;
    for (int it = 0; (it = table_iterate(&indirect_function_table, it, &name, (void**)&info)) != -1;
         ++index)
      indirect_funcs[index] = info;

    qsort(indirect_funcs, count, sizeof(*indirect_funcs), compare_indirect);

    data_leb128(&elems_section, -1, 1);  // num elem segments
    data_leb128(&elems_section, -1, 0);  // segment flags
    data_push(&elems_section, OP_I32_CONST);
    data_leb128(&elems_section, -1, table_start_index);  // start index
    data_push(&elems_section, OP_END);
    data_leb128(&elems_section, -1, count);  // num elems
    for (int i = 0; i < count; ++i) {
      FuncInfo *info = indirect_funcs[i];
      VERBOSE("%2d: %.*s (%d)\n", i + 1, NAMES(info->func->name), (int)info->index);
      data_leb128(&elems_section, -1, info->index);  // elem function index
    }
    VERBOSES("\n");
  }

  // Tag.
  DataStorage tag_section;
  data_init(&tag_section);
  if (tags->len > 0) {
    for (int i = 0; i < tags->len; ++i) {
      int typeindex = VOIDP2INT(tags->data[i]);
      int attribute = 0;
      data_uleb128(&tag_section, -1, attribute);
      data_uleb128(&tag_section, -1, typeindex);
    }
    data_uleb128(&tag_section, 0, tags->len);  // tag count
    data_uleb128(&tag_section, 0, tag_section.len);  // Size
  }

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

  // Table.
  if (table_section.len > 0) {
    data_push(&sections, SEC_TABLE);  // Section "Table" (4)
    data_uleb128(&sections, -1, table_section.len);
    data_append(&sections, table_section.buf, table_section.len);
  }

  // Memory.
  if (memory_section.len > 0) {
    data_push(&sections, SEC_MEMORY);  // Section "Memory" (5)
    data_uleb128(&sections, -1, memory_section.len);
    data_append(&sections, memory_section.buf, memory_section.len);
  }

  // Tag (must put earlier than Global section.)
  if (tag_section.len > 0) {
    data_push(&sections, SEC_TAG);  // Section "Tag" (13)
    data_append(&sections, tag_section.buf, tag_section.len);
  }

  // Globals
  if (globals_count > 0) {
    data_push(&sections, SEC_GLOBAL);  // Section "Global" (6)
    data_append(&sections, globals_section.buf, globals_section.len);
  }

  // Exports
  data_push(&sections, SEC_EXPORT);  // Section "Export" (7)
  data_append(&sections, exports_section.buf, exports_section.len);

  // Elements
  if (elems_section.len > 0) {
    data_push(&sections, SEC_ELEM);  // Section "Elem" (9)
    data_uleb128(&sections, -1, elems_section.len);
    data_append(&sections, elems_section.buf, elems_section.len);
  }

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
      DataStorage *code = ((FuncExtra*)func->extra)->code;
      // function body i.
      total_code_size += code->len;
    }
  }

  // Put num function first, and insert total size after.
  data_uleb128(&codesec, -1, function_count);  // num functions
  data_uleb128(&codesec, size_pos, (codesec.len - size_pos) + total_code_size);  // Size
  fwrite(codesec.buf, codesec.len, 1, ofp);

  {
    const Name *name;
    FuncInfo *info;
    for (int it = 0; (it = table_iterate(&func_info_table, it, &name, (void**)&info)) != -1; ) {
      Function *func = info->func;
      if (func == NULL || info->flag == 0)
        continue;
      DataStorage *code = ((FuncExtra*)func->extra)->code;
      fwrite(code->buf, code->len, 1, ofp);
    }
  }

  // Data
  {
    DataStorage datasec;
    data_init(&datasec);
    construct_data_segment(&datasec);
    if (datasec.len > 0) {
      static const unsigned char seg_info[] = {
        0x01,             // num data segments
        0x00,             // segment flags
        OP_I32_CONST, 0,  // i32.const 0
        OP_END,
      };
      size_t datasize = datasec.len;
      data_insert(&datasec, 0, seg_info, sizeof(seg_info));
      data_uleb128(&datasec, sizeof(seg_info), datasize);  // data segment size

      static const unsigned char sec[] = {SEC_DATA};
      size_t section_size = datasec.len;
      data_insert(&datasec, 0, sec, sizeof(sec));
      data_uleb128(&datasec, sizeof(sec), section_size);  // section size

      fwrite(datasec.buf, datasec.len, 1, ofp);
    }
  }
}
