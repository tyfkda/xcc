#include "wcc.h"

#include <assert.h>
#include <limits.h>  // CHAR_BIT
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "lexer.h"
#include "parser.h"
#include "preprocessor.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "wasm_util.h"

static const char IMPORT_MODULE_NAME[] = "c";
static const char IMPORT_MODULE_ENV_NAME[] = "env";

uint32_t stack_size = 8 * 1024;
int error_count;
bool verbose;

////////////////////////////////////////////////

static void construct_primitive_global(DataStorage *ds, const VarInfo *varinfo) {
  const Type *type = varinfo->type;
  Initializer *init = varinfo->global.init;
  switch (type->kind) {
  case TY_FIXNUM:
  case TY_PTR:
    {
      Fixnum v = 0;
      if (init != NULL) {
        assert(init->kind == IK_SINGLE);
        Expr *value = init->single;
        switch (value->kind) {
        case EX_FIXNUM:
          v = value->fixnum;
          break;
        case EX_REF:
          {
            Expr *sub = value->unary.sub;
            assert(sub->kind == EX_VAR);
            if (sub->type->kind == TY_FUNC) {
              v = get_indirect_function_index(sub->var.name);
            } else {
              const GVarInfo *info = get_gvar_info(sub);
              assert(info->varinfo->storage & VS_REF_TAKEN);
              v = info->non_prim.address;
            }
          }
          break;
        case EX_VAR:
          if (value->type->kind == TY_FUNC) {
            v = get_indirect_function_index(value->var.name);
          } else {
            GVarInfo *info = get_gvar_info(value);
            assert(!is_prim_type(info->varinfo->type) || (info->varinfo->storage & VS_REF_TAKEN));
            v = info->non_prim.address;
          }
          break;
        default: assert(false); break;
        }
      }
      data_push(ds, type_size(type) <= I32_SIZE ? OP_I32_CONST : OP_I64_CONST);
      emit_leb128(ds, ds->len, v);
    }
    break;
#ifndef __NO_FLONUM
  case TY_FLONUM:
    {
      double v = 0;
      if (init != NULL) {
        assert(init->kind == IK_SINGLE);
        Expr *expr = init->single;
        switch (expr->kind) {
        case EX_FLONUM:
          v = expr->flonum;
          break;
        default: assert(false); break;
        }
      }
      if (type->flonum.kind < FL_DOUBLE) {
        data_push(ds, OP_F32_CONST);
        float f = v;
        data_append(ds, (unsigned char*)&f, sizeof(float));  // TODO: Endian
      } else {
        data_push(ds, OP_F64_CONST);
        double d = v;
        data_append(ds, (unsigned char*)&d, sizeof(double));  // TODO: Endian
      }
    }
    break;
#endif
  default: assert(false); break;
  }
}

static void construct_initial_value(DataStorage *ds, const Type *type, const Initializer *init) {
  assert(init == NULL || init->kind != IK_DOT);

  switch (type->kind) {
  case TY_FIXNUM:
  case TY_PTR:
    {
      Fixnum v = 0;
      if (init != NULL) {
        assert(init->kind == IK_SINGLE);
        Expr *value = init->single;
        switch (value->kind) {
        case EX_FIXNUM:
          v = value->fixnum;
          break;
        case EX_REF:
          {
            Expr *sub = value->unary.sub;
            if (sub->type->kind == TY_FUNC) {
              v = get_indirect_function_index(sub->var.name);
            } else {
              assert(sub->kind == EX_VAR);
              const GVarInfo *info = get_gvar_info(sub);
              assert(info->varinfo->storage & VS_REF_TAKEN);
              v = info->non_prim.address;
            }
          }
          break;
        case EX_VAR:
          if (value->type->kind == TY_FUNC) {
            v = get_indirect_function_index(value->var.name);
          } else {
            assert(is_global_scope(value->var.scope));
            GVarInfo *info = get_gvar_info(value);
            assert(!is_prim_type(info->varinfo->type) || (info->varinfo->storage & VS_REF_TAKEN));
            v = info->non_prim.address;
          }
          break;
        default: assert(false); break;
        }
      }

      unsigned char buf[16];
      int size = type_size(type);
      assert(size <= (int)sizeof(buf));
      for (int i = 0; i < size; ++i)
        buf[i] = v >> (i * CHAR_BIT);  // Little endian

      data_append(ds, buf, size);
    }
    break;
#ifndef __NO_FLONUM
  case TY_FLONUM:
    {
      double v = 0;
      if (init != NULL) {
        assert(init->kind == IK_SINGLE);
        Expr *expr = init->single;
        switch (expr->kind) {
        case EX_FLONUM:
          v = expr->flonum;
          break;
        default: assert(false); break;
        }
      }
      if (type->flonum.kind < FL_DOUBLE) {
        float f = v;
        data_append(ds, (unsigned char*)&f, sizeof(float));  // TODO: Endian
      } else {
        double d = v;
        data_append(ds, (unsigned char*)&d, sizeof(double));  // TODO: Endian
      }
    }
    break;
#endif
  case TY_ARRAY:
    if (init == NULL || init->kind == IK_MULTI) {
      const Type *elem_type = type->pa.ptrof;
      //size_t elem_size = type_size(elem_type);
      if (init != NULL) {
        Vector *init_array = init->multi;
        size_t index = 0;
        size_t len = init_array->len;
        for (size_t i = 0; i < len; ++i, ++index) {
          const Initializer *init_elem = init_array->data[i];
          if (init_elem->kind == IK_ARR) {
            size_t next = init_elem->arr.index->fixnum;
            for (size_t j = index; j < next; ++j)
              construct_initial_value(ds, elem_type, NULL);
            index = next;
            init_elem = init_elem->arr.value;
          }
          construct_initial_value(ds, elem_type, init_elem);
        }
        // Padding
        for (size_t i = index, n = type->pa.length; i < n; ++i)
          construct_initial_value(ds, elem_type, NULL);
      }
    } else {
      if (init->kind == IK_SINGLE && is_char_type(type->pa.ptrof) && init->single->kind == EX_STR) {
        size_t src_size = init->single->str.size;
        size_t size = type_size(type);
        assert(size >= (size_t)src_size);
        if (size > src_size) {
          unsigned char *buf = calloc(1, size);
          assert(buf != NULL);
          memcpy(buf, init->single->str.buf, src_size);
          data_append(ds, buf, size);
          free(buf);
        } else {
          data_append(ds, (unsigned char*)init->single->str.buf, src_size);
        }
      } else {
        error("Illegal initializer");
      }
    }
    break;
  case TY_STRUCT:
    {
      assert(init == NULL || init->kind == IK_MULTI);

      const StructInfo *sinfo = type->struct_.info;
      int count = 0;
      int offset = 0;
      for (int i = 0, n = sinfo->members->len; i < n; ++i) {
        const VarInfo* member = sinfo->members->data[i];
        const Initializer *mem_init;
        if (init == NULL) {
          if (sinfo->is_union)
            continue;
          mem_init = NULL;
        } else {
          mem_init = init->multi->data[i];
        }
        if (mem_init != NULL || !sinfo->is_union) {
          int align = align_size(member->type);
          int d = offset % align;
          if (d > 0) {
            d = align - d;
            for (int i = 0; i < d; ++i)
              data_push(ds, 0);
            offset += d;
          }
          construct_initial_value(ds, member->type, mem_init);
          ++count;
          offset = ALIGN(offset, align);
          offset += type_size(member->type);
        }
      }
      if (sinfo->is_union && count <= 0) {
        const VarInfo* member = sinfo->members->data[0];
        construct_initial_value(ds, member->type, NULL);
        offset += type_size(member->type);
      }

      size_t size = type_size(type);
      if (size != (size_t)offset) {
        assert(size > (size_t)offset);
        // Put padding.
        int d = size - offset;
        for (int i = 0; i < d; ++i)
          data_push(ds, 0);
      }
    }
    break;
  default: assert(false); break;
  }
}

static void construct_data_segment(DataStorage *ds) {
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
        zerobuf = realloc(zerobuf, sz);
        if (zerobuf == NULL)
          error("Out of memory");
        memset(zerobuf + zerosz, 0x00, sz - zerosz);
        zerosz = sz;
      }
      data_append(ds, zerobuf, sz);
    }

    construct_initial_value(ds, varinfo->type, varinfo->global.init);

    address = adr + type_size(varinfo->type);
  }
}

static int compare_indirect(const void *pa, const void *pb) {
  const FuncInfo *qa = *(const FuncInfo**)pa;
  const FuncInfo *qb = *(const FuncInfo**)pb;
  return (int)qa->indirect_index - (int)qb->indirect_index;
}

static void emit_wasm(FILE *ofp, Vector *exports) {
  emit_wasm_header(ofp);

  // Types.
  DataStorage types_section;
  data_init(&types_section);
  for (int i = 0, len = functypes->len; i < len; ++i) {
    const Type *type = functypes->data[i];
    data_push(&types_section, WT_FUNC);  // func
    const Vector *params = type->func.params;
    int param_count = params != NULL ? params->len : 0;
    if (type->func.vaargs)
      ++param_count;
    emit_uleb128(&types_section, types_section.len, param_count);  // num params
    for (int i = 0; i < param_count; ++i) {
      if (type->func.vaargs && i == param_count - 1) {
        data_push(&types_section, to_wtype(&tyVoidPtr));  // vaarg pointer.
      } else {
        VarInfo *varinfo = params->data[i];
        assert(is_prim_type(varinfo->type));
        data_push(&types_section, to_wtype(varinfo->type));
      }
    }
    if (type->func.ret->kind == TY_VOID) {
      data_push(&types_section, 0);  // num results
    } else {
      assert(is_prim_type(type->func.ret));
      data_push(&types_section, 1);  // num results
      data_push(&types_section, to_wtype(type->func.ret));
    }
  }
  emit_uleb128(&types_section, 0, functypes->len);  // num types
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
      emit_uleb128(&imports_section, imports_section.len, IMPORT_FUNC);  // import kind
      emit_uleb128(&imports_section, imports_section.len, type_index);  // import signature index
      ++imports_count;
    }
  }
  {  // TODO: If needed.
    // Import memory "env.memory".
    const char *module_name = IMPORT_MODULE_ENV_NAME;
    size_t module_name_len = strlen(module_name);
    emit_uleb128(&imports_section, imports_section.len, module_name_len);  // string length
    data_append(&imports_section, (const unsigned char*)module_name, module_name_len);  // import module name
    const char *name = "memory";
    size_t name_len = strlen(name);
    emit_uleb128(&imports_section, imports_section.len, name_len);  // string length
    data_append(&imports_section, (const unsigned char*)name, name_len);  // import name
    emit_uleb128(&imports_section, imports_section.len, IMPORT_MEMORY);  // import kind (function)
    emit_uleb128(&imports_section, imports_section.len, 0);  // limits: flags
    emit_uleb128(&imports_section, imports_section.len, 1);  // limits: initial
    ++imports_count;
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

  // Table.
  DataStorage table_section;
  data_init(&table_section);
  // TODO: Output table only when it is needed.
  int table_start_index = 1;
  emit_leb128(&table_section, table_section.len, 1);  // num tables
  data_push(&table_section, WT_FUNCREF);
  data_push(&table_section, 0x00);  // limits: flags
  emit_leb128(&table_section, table_section.len, table_start_index + indirect_function_table.count);  // initial

  // Elements.
  DataStorage elems_section;
  data_init(&elems_section);
  if (indirect_function_table.count > 0) {
    int count = indirect_function_table.count;
    FuncInfo **indirect_funcs = malloc(sizeof(*indirect_funcs) * count);

    // Enumerate imported functions.
    VERBOSES("### Indirect functions\n");
    const Name *name;
    FuncInfo *info;
    int index = 0;
    for (int it = 0; (it = table_iterate(&indirect_function_table, it, &name, (void**)&info)) != -1; ++index)
      indirect_funcs[index] = info;

    QSORT(indirect_funcs, count, sizeof(*indirect_funcs), compare_indirect);

    emit_leb128(&elems_section, elems_section.len, 1);  // num elem segments
    emit_leb128(&elems_section, elems_section.len, 0);  // segment flags
    data_push(&elems_section, OP_I32_CONST);
    emit_leb128(&elems_section, elems_section.len, table_start_index);  // start index
    data_push(&elems_section, OP_END);
    emit_leb128(&elems_section, elems_section.len, count);  // num elems
    for (int i = 0 ; i < count; ++i) {
      FuncInfo *info = indirect_funcs[i];
      VERBOSE("%2d: %.*s (%d)\n", i, info->func->name->bytes, info->func->name->chars, (int)info->index);
      emit_leb128(&elems_section, elems_section.len, info->index);  // elem function index
    }
    free(indirect_funcs);
    VERBOSES("\n");
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
    emit_uleb128(&globals_section, 0, globals_count);  // num globals
    emit_uleb128(&globals_section, 0, globals_section.len);  // Size
  }

  // Exports.
  DataStorage exports_section;
  data_init(&exports_section);
  int num_exports = 0;
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
    emit_uleb128(&exports_section, exports_section.len, IMPORT_FUNC);  // export kind
    emit_uleb128(&exports_section, exports_section.len, func_index);  // export func index
    ++num_exports;
  }
  // Export globals.
  {
    const Name *name;
    GVarInfo *info;
    for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
      const VarInfo *varinfo = info->varinfo;
      if (!info->export || !is_prim_type(varinfo->type) || (varinfo->storage & VS_REF_TAKEN))
        continue;
      emit_uleb128(&exports_section, exports_section.len, name->bytes);  // string length
      data_append(&exports_section, (const unsigned char*)name->chars, name->bytes);  // export name
      emit_uleb128(&exports_section, exports_section.len, EXPORT_GLOBAL);  // export kind
      emit_uleb128(&exports_section, exports_section.len, info->prim.index);  // export global index
      ++num_exports;
    }
  }
  emit_uleb128(&exports_section, 0, num_exports);  // num exports
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

  // Table.
  if (table_section.len > 0) {
    data_push(&sections, SEC_TABLE);  // Section "Table" (4)
    emit_uleb128(&sections, sections.len, table_section.len);
    data_append(&sections, table_section.buf, table_section.len);
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
    emit_uleb128(&sections, sections.len, elems_section.len);
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
      emit_uleb128(&datasec, sizeof(seg_info), datasize);  // data segment size

      static const unsigned char sec[] = {SEC_DATA};
      size_t section_size = datasec.len;
      data_insert(&datasec, 0, sec, sizeof(sec));
      emit_uleb128(&datasec, sizeof(sec), section_size);  // section size

      fwrite(datasec.buf, datasec.len, 1, ofp);
    }
  }
}

////////////////////////////////////////////////

static Expr *proc_builtin_va_start(const Token *ident) {
  if (curfunc == NULL || !curfunc->type->func.vaargs) {
    parse_error(ident, "`va_start' can only be used in a variadic function");
    return NULL;
  }

  consume(TK_LPAR, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);
  if (args == NULL || args->len != 2) {
    parse_error(token, "two arguments expected");
    return NULL;
  }

  //#define va_start(ap, param)  (void)(ap = __va_args__)

  const Type *tyvalist = find_typedef(curscope, alloc_name("__builtin_va_list", NULL, false), NULL);
  assert(tyvalist != NULL);

  Expr *ap = args->data[0];
  Expr *param = args->data[1];
  if (param->kind != EX_VAR)
    parse_error(param->token, "variable expected");
  const Type *functype = curfunc->type;
  const Vector *funparams = functype->func.params;
  if (funparams == NULL ||
      !equal_name(((VarInfo*)funparams->data[funparams->len - 1])->name, param->var.name)) {
    parse_error(param->token, "must be the last parameter");
    return NULL;
  }

  Scope *top_scope = curscope;
  for (Scope *p = curscope; p = p->parent, !is_global_scope(p); )
    top_scope = p;

  const Name *name = alloc_name(VA_ARGS_NAME, NULL, false);
  Expr *va_args = new_expr_variable(name, tyvalist, param->token, top_scope);
  Expr *assign = new_expr_bop(EX_ASSIGN, ap->type, ap->token, ap, va_args);
  return new_expr_cast(&tyVoid, ident, assign);
}

static Expr *proc_builtin_va_end(const Token *ident) {
  consume(TK_LPAR, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);
  if (args == NULL || args->len != 1) {
    parse_error(token, "one arguments expected");
    return NULL;
  }

  //#define va_end(ap)           (void)(ap = 0)

  Expr *ap = args->data[0];
  Expr *assign = new_expr_bop(EX_ASSIGN, ap->type, ap->token, ap,
                              new_expr_fixlit(&tyInt, ident, 0));
  return new_expr_cast(&tyVoid, ident, assign);
}

static Expr *proc_builtin_va_arg(const Token *ident) {
  consume(TK_LPAR, "`(' expected");
  Expr *ap = parse_assign();
  consume(TK_COMMA, "`,' expected");
  const Type *type = parse_full_type(NULL, NULL);
  consume(TK_RPAR, "`)' expected");

  //#define va_arg(v,l)     (ap = (char*)ap + sizeof(type), *(type*)((char*)ap - sizeof(type)))

  size_t size = type_size(type);
  Expr *size_lit = new_expr_fixlit(&tySize, ap->token, size);
  Expr *cap = make_cast(ptrof(&tyUnsignedChar), ap->token, ap, true);
  Expr *add = new_expr_bop(EX_ASSIGN, &tyVoid, ap->token, ap,
                           new_expr_bop(EX_ADD, cap->type, cap->token, cap, size_lit));
  Expr *deref = new_expr_deref(
      ap->token,
      make_cast(ptrof(type), ap->token,
                new_expr_bop(EX_SUB, cap->type, cap->token, cap, size_lit),
                true));
  return new_expr_bop(EX_COMMA, type, ident, add, deref);
}

static Expr *proc_builtin_va_copy(const Token *ident) {
  consume(TK_LPAR, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);
  if (args == NULL || args->len != 2) {
    parse_error(token, "two arguments expected");
    return NULL;
  }

  //#define va_start(ap, param)  (void)(ap = (va_list)&(param))

  Expr *dst = args->data[0];
  Expr *src = args->data[1];
  Expr *assign = new_expr_bop(EX_ASSIGN, dst->type, dst->token, dst, src);
  return new_expr_cast(&tyVoid, ident, assign);
}

static void install_builtins(void) {
  // __builtin_va_list
  {
    const Type *type = ptrof(&tyVoidPtr);
    const Name *name = alloc_name("__builtin_va_list", NULL, false);
    add_typedef(global_scope, name, type);
  }

  static BuiltinExprProc p_va_start = &proc_builtin_va_start;
  static BuiltinExprProc p_va_end = &proc_builtin_va_end;
  static BuiltinExprProc p_va_arg = &proc_builtin_va_arg;
  static BuiltinExprProc p_va_copy = &proc_builtin_va_copy;

  add_builtin_expr_ident("__builtin_va_start", &p_va_start);
  add_builtin_expr_ident("__builtin_va_end", &p_va_end);
  add_builtin_expr_ident("__builtin_va_arg", &p_va_arg);
  add_builtin_expr_ident("__builtin_va_copy", &p_va_copy);
}

static void init_compiler(void) {
  table_init(&func_info_table);
  functypes = new_vector();
  table_init(&indirect_function_table);
  init_lexer();
  init_global();

  set_fixnum_size(FX_CHAR,  1, 1);
  set_fixnum_size(FX_SHORT, 2, 2);
  set_fixnum_size(FX_INT,   4, 4);
  set_fixnum_size(FX_LONG,  4, 4);
  set_fixnum_size(FX_LLONG, 8, 8);
  set_fixnum_size(FX_ENUM,  4, 4);

  install_builtins();
}

static void compile1(FILE *ifp, const char *filename, Vector *decls) {
  set_source_file(ifp, filename);
  parse(decls);
}

int main(int argc, char *argv[]) {
  const char *ofn = "a.wasm";
  Vector *exports = NULL;
  int iarg;

  FILE *ppout = tmpfile();
  if (ppout == NULL)
    error("cannot open temporary file");

  init_preprocessor(ppout);
  define_macro_simple("__ILP32__");
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
    } else if (starts_with(arg, "-I")) {
      add_system_inc_path(arg + 2);
    } else if (starts_with(argv[iarg], "-D")) {
      define_macro(arg + 2);
    } else if (strncmp(arg, "--stack-size=", 13) == 0) {
      int size = atoi(arg + 13);
      if (size <= 0) {
        error("stack-size must be positive");
      }
      stack_size = size;
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

  // Preprocess.
  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      const char *filename = argv[i];
      FILE *ifp = fopen(filename, "r");
      if (ifp == NULL)
        error("Cannot open file: %s\n", filename);
      fprintf(ppout, "# 1 \"%s\" 1\n", filename);
      preprocess(ifp, filename);
      fclose(ifp);
    }
  } else {
    preprocess(stdin, "*stdin*");
  }
  if (fseek(ppout, 0, SEEK_SET) != 0) {
    error("fseek failed");
    return 1;
  }

  // Compile.
  toplevel = new_vector();
  FILE *ppin = ppout;
  compile1(ppin, "*", toplevel);
  fclose(ppin);

  traverse_ast(toplevel, exports);

  gen(toplevel);
  if (error_count != 0)
    return 1;

  FILE *fp = fopen(ofn, "wb");
  if (fp == NULL) {
    error("Cannot open output file");
  } else {
    emit_wasm(fp, exports);
    assert(error_count == 0);
    fclose(fp);
  }

  return 0;
}
