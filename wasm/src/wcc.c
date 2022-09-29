#include "wcc.h"

#include <assert.h>
#include <libgen.h>  // dirname
#include <limits.h>  // CHAR_BIT
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

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
static const uint32_t DEFAULT_STACK_SIZE = 8 * 1024;
static const uint32_t MEMORY_PAGE_SIZE = 65536;

bool verbose;

////////////////////////////////////////////////

static void eval_initial_value(Expr *expr, Expr **pvar, Fixnum *poffset) {
  switch (expr->kind) {
  case EX_FIXNUM:
    if (!is_const(expr))
      assert(!"initializer type error");
    *poffset = expr->fixnum;
    break;
  case EX_VAR:
    assert(*pvar == NULL);
    *pvar = expr;
    break;
  case EX_ADD:
  case EX_SUB:
    {
      Expr *var1 = NULL, *var2 = NULL;
      Fixnum offset1 = 0, offset2 = 0;
      eval_initial_value(expr->bop.lhs, &var1, &offset1);
      eval_initial_value(expr->bop.rhs, &var2, &offset2);
      if (var1 != NULL) {
        assert(var2 == NULL);
        *pvar = var1;
      } else if (var2 != NULL) {
        assert(expr->kind == EX_ADD);
        *pvar = var2;
      }
      if (expr->kind == EX_SUB)
        offset2 = -offset2;
      *poffset = offset1 + offset2;
    }
    break;
  case EX_REF:
  case EX_DEREF:
  case EX_CAST:
    eval_initial_value(expr->unary.sub, pvar, poffset);
    return;
  case EX_MEMBER:
    {
      eval_initial_value(expr->member.target, pvar, poffset);

      const Type *type = expr->member.target->type;
      if (ptr_or_array(type))
        type = type->pa.ptrof;
      assert(type->kind == TY_STRUCT);
      const Vector *members = type->struct_.info->members;
      const MemberInfo *member = members->data[expr->member.index];
      *poffset += member->offset;
    }
    break;
  case EX_COMPLIT:
    assert(expr->complit.var->kind == EX_VAR);
    eval_initial_value(expr->complit.var, pvar, poffset);
    break;
  default: assert(false); break;
  }
}

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
        Expr *var = NULL;
        Fixnum offset = 0;
        eval_initial_value(init->single, &var, &offset);
        if (var != NULL) {
          if (var->type->kind == TY_FUNC) {
            assert(offset == 0);
            v = get_indirect_function_index(var->var.name);
          } else {
            GVarInfo *info = get_gvar_info(var);
            assert(!is_prim_type(info->varinfo->type) || (info->varinfo->storage & VS_REF_TAKEN));
            v = info->non_prim.address;
          }
        }
        v += offset;
      }
      data_push(ds, type_size(type) <= I32_SIZE ? OP_I32_CONST : OP_I64_CONST);
      emit_leb128(ds, -1, v);
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
      Expr *var = NULL;
      Fixnum v = 0;
      if (init != NULL) {
        assert(init->kind == IK_SINGLE);
        eval_initial_value(init->single, &var, &v);
      }
      if (var != NULL) {
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
      size_t index = 0;
      if (init != NULL) {
        Vector *init_array = init->multi;
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
      }
      // Padding
      for (size_t i = index, n = type->pa.length; i < n; ++i)
        construct_initial_value(ds, elem_type, NULL);
      break;
    }
    if (init->kind == IK_SINGLE && is_char_type(type->pa.ptrof)) {
      Expr *e = strip_cast(init->single);
      if (e->kind == EX_STR) {
        size_t src_size = e->str.size;
        size_t size = type_size(type);
        if (size > src_size) {
          unsigned char *buf = calloc(1, size);
          assert(buf != NULL);
          memcpy(buf, init->single->str.buf, src_size);
          data_append(ds, buf, size);
          free(buf);
        } else {
          data_append(ds, (unsigned char*)e->str.buf, size);
        }
        break;
      }
    }
    error("Illegal initializer");
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
    assert(ds->len == address);
  }
}

static int compare_indirect(const void *pa, const void *pb) {
  const FuncInfo *qa = *(const FuncInfo**)pa;
  const FuncInfo *qb = *(const FuncInfo**)pb;
  return (int)qa->indirect_index - (int)qb->indirect_index;
}

static void emit_wasm(FILE *ofp, Vector *exports, uint32_t address_bottom) {
  emit_wasm_header(ofp);

  // Types.
  DataStorage types_section;
  data_init(&types_section);
  for (int i = 0, len = functypes->len; i < len; ++i) {
    const WasmFuncType *wt = functypes->data[i];
    data_push(&types_section, WT_FUNC);  // func
    data_append(&types_section, wt->buf, wt->size);
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

      emit_uleb128(&imports_section, -1, module_name_len);  // string length
      data_append(&imports_section, (const unsigned char*)module_name, module_name_len);  // import module name
      int name_len = name->bytes;
      emit_uleb128(&imports_section, -1, name_len);  // string length
      data_append(&imports_section, (const unsigned char*)name->chars, name_len);  // import name
      data_push(&imports_section, IMPORT_FUNC);  // import kind
      emit_uleb128(&imports_section, -1, type_index);  // import signature index
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
      emit_uleb128(&functions_section, -1, type_index);  // function i signature index
    }
  }
  emit_uleb128(&functions_section, 0, function_count);  // num functions
  emit_uleb128(&functions_section, 0, functions_section.len);  // Size

  // Table.
  DataStorage table_section;
  data_init(&table_section);
  // TODO: Output table only when it is needed.
  int table_start_index = 1;
  emit_leb128(&table_section, -1, 1);  // num tables
  data_push(&table_section, WT_FUNCREF);
  data_push(&table_section, 0x00);  // limits: flags
  emit_leb128(&table_section, -1, table_start_index + indirect_function_table.count);  // initial

  // Memory.
  DataStorage memory_section;
  data_init(&memory_section);
  {
    uint32_t page_count = (address_bottom + MEMORY_PAGE_SIZE - 1) / MEMORY_PAGE_SIZE;
    if (page_count <= 0)
      page_count = 1;
    emit_uleb128(&memory_section, -1, 1);  // count?
    emit_uleb128(&memory_section, -1, 0);  // index?
    emit_uleb128(&memory_section, -1, page_count);
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
    emit_uleb128(&exports_section, -1, name_len);  // string length
    data_append(&exports_section, (const unsigned char*)name->chars, name_len);  // export name
    emit_uleb128(&exports_section, -1, IMPORT_FUNC);  // export kind
    emit_uleb128(&exports_section, -1, func_index);  // export func index
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
      emit_uleb128(&exports_section, -1, name->bytes);  // string length
      data_append(&exports_section, (const unsigned char*)name->chars, name->bytes);  // export name
      data_push(&exports_section, EXPORT_GLOBAL);  // export kind
      emit_uleb128(&exports_section, -1, info->prim.index);  // export global index
      ++num_exports;
    }
  }
  /*if (memory_section.len > 0)*/ {  // TODO: Export only if memory exists
    static const char name[] = "memory";
    emit_uleb128(&exports_section, -1, sizeof(name) - 1);  // string length
    data_append(&exports_section, (const unsigned char*)name, sizeof(name) - 1);  // export name
    emit_uleb128(&exports_section, -1, IMPORT_MEMORY);  // export kind
    emit_uleb128(&exports_section, -1, 0);  // export global index
    ++num_exports;
  }
  emit_uleb128(&exports_section, 0, num_exports);  // num exports
  emit_uleb128(&exports_section, 0, exports_section.len);  // Size

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

    qsort(indirect_funcs, count, sizeof(*indirect_funcs), compare_indirect);

    emit_leb128(&elems_section, -1, 1);  // num elem segments
    emit_leb128(&elems_section, -1, 0);  // segment flags
    data_push(&elems_section, OP_I32_CONST);
    emit_leb128(&elems_section, -1, table_start_index);  // start index
    data_push(&elems_section, OP_END);
    emit_leb128(&elems_section, -1, count);  // num elems
    for (int i = 0 ; i < count; ++i) {
      FuncInfo *info = indirect_funcs[i];
      VERBOSE("%2d: %.*s (%d)\n", i, info->func->name->bytes, info->func->name->chars, (int)info->index);
      emit_leb128(&elems_section, -1, info->index);  // elem function index
    }
    free(indirect_funcs);
    VERBOSES("\n");
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
    emit_uleb128(&sections, -1, table_section.len);
    data_append(&sections, table_section.buf, table_section.len);
  }

  // Memory.
  if (memory_section.len > 0) {
    data_push(&sections, SEC_MEMORY);  // Section "Memory" (5)
    emit_uleb128(&sections, -1, memory_section.len);
    data_append(&sections, memory_section.buf, memory_section.len);
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
    emit_uleb128(&sections, -1, elems_section.len);
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
  emit_uleb128(&codesec, -1, function_count);  // num functions
  emit_uleb128(&codesec, size_pos, (codesec.len - size_pos) + total_code_size);  // Size
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
    parse_error(PE_FATAL, ident, "`va_start' can only be used in a variadic function");
    return NULL;
  }

  consume(TK_LPAR, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);
  if (args == NULL || args->len != 2) {
    parse_error(PE_FATAL, token, "two arguments expected");
    return NULL;
  }

  //#define va_start(ap, param)  (void)(ap = __va_args__)

  Type *tyvalist = find_typedef(curscope, alloc_name("__builtin_va_list", NULL, false), NULL);
  assert(tyvalist != NULL);

  Expr *ap = args->data[0];
  Expr *param = args->data[1];
  if (param->kind != EX_VAR)
    parse_error(PE_FATAL, param->token, "variable expected");
  const Type *functype = curfunc->type;
  const Vector *funparams = functype->func.params;
  if (funparams == NULL ||
      !equal_name(((VarInfo*)funparams->data[funparams->len - 1])->name, param->var.name)) {
    parse_error(PE_FATAL, param->token, "must be the last parameter");
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
    parse_error(PE_FATAL, token, "one arguments expected");
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
  Type *type = parse_var_def(NULL, NULL, NULL);
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
    parse_error(PE_FATAL, token, "two arguments expected");
    return NULL;
  }

  //#define va_start(ap, param)  (void)(ap = (va_list)&(param))

  Expr *dst = args->data[0];
  Expr *src = args->data[1];
  Expr *assign = new_expr_bop(EX_ASSIGN, dst->type, dst->token, dst, src);
  return new_expr_cast(&tyVoid, ident, assign);
}

static void gen_builtin_memory_grow(Expr *expr) {
  assert(expr->kind == EX_FUNCALL);
  Vector *args = expr->funcall.args;
  assert(args->len == 1);

  gen_expr(args->data[0], true);
  ADD_CODE(OP_MEMORY_GROW, 0x00);
}

static void install_builtins(void) {
  // __builtin_va_list
  {
    Type *type = ptrof(&tyVoidPtr);
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

  {
    static BuiltinFunctionProc p_memory_grow = &gen_builtin_memory_grow;
    Vector *params = new_vector();
    var_add(params, NULL, &tySize, 0);

    Type *rettype = &tyInt;
    Vector *param_types = extract_varinfo_types(params);
    Type *type = new_func_type(rettype, params, param_types, false);

    add_builtin_function("__builtin_memory_grow", type, &p_memory_grow, true);
  }
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

static bool add_lib(Vector *lib_paths, const char *fn, Vector *sources) {
  for (int i = 0; i < lib_paths->len; ++i) {
    char *path = cat_path(lib_paths->data[i], fn);
    FILE *fp = fopen(path, "r");  // TODO: use stat.
    if (fp != NULL) {
      fclose(fp);
      vec_push(sources, path);
      return true;
    }
    free(path);
  }
  return false;
}

int main(int argc, char *argv[]) {
  const char *root = dirname(strdup(argv[0]));
  if (!is_fullpath(root)) {
    char *cwd = getcwd(NULL, 0);
    root = cat_path(cwd, root);
  }

  const char *ofn = "a.wasm";
  Vector *exports = new_vector();
  uint32_t stack_size = DEFAULT_STACK_SIZE;
  const char *entry_point = "_start";
  bool nodefaultlibs = false, nostdlib = false;
  Vector *lib_paths = new_vector();

  FILE *ppout = tmpfile();
  if (ppout == NULL)
    error("cannot open temporary file");

  init_preprocessor(ppout);
  define_macro("__ILP32__");
  define_macro("__WASM");
  add_system_inc_path(cat_path(root, "include"));
  add_system_inc_path(cat_path(root, "../include"));

  init_compiler();

  enum {
    OPT_VERBOSE = 256,
    OPT_ENTRY_POINT,
    OPT_STACK_SIZE,
    OPT_NODEFAULTLIBS,
    OPT_NOSTDLIB,

    OPT_WARNING,
    OPT_OPTIMIZE,
    OPT_DEBUGINFO,
    OPT_ANSI,
    OPT_STD,
    OPT_PEDANTIC,
    OPT_MMD,
  };
  static const struct option options[] = {
    {"I", required_argument},  // Add include path
    {"L", required_argument},  // Add library path
    {"D", required_argument},  // Define macro
    {"o", required_argument},  // Specify output filename
    {"e", required_argument},  // Export names
    {"W", required_argument, OPT_WARNING},
    {"nodefaultlibs", no_argument, OPT_NODEFAULTLIBS},
    {"nostdlib", no_argument, OPT_NOSTDLIB},
    {"-verbose", no_argument, OPT_VERBOSE},
    {"-entry-point", required_argument, OPT_ENTRY_POINT},
    {"-stack-size", required_argument, OPT_STACK_SIZE},

    // Suppress warnings
    {"O", required_argument, OPT_OPTIMIZE},
    {"g", required_argument, OPT_DEBUGINFO},
    {"ansi", no_argument, OPT_ANSI},
    {"std", required_argument, OPT_STD},
    {"pedantic", no_argument, OPT_PEDANTIC},
    {"MMD", no_argument, OPT_MMD},

    {NULL},
  };
  int opt;
  while ((opt = optparse(argc, argv, options)) != -1) {
    switch (opt) {
    case 'o':
      ofn = optarg;
      break;
    case 'e':
      if (*optarg != '\0') {
        const char *s = optarg;
        for (;;) {
          const char *p = strchr(s, ',');
          const Name *name = alloc_name(s, p, false);
          vec_push(exports, name);
          if (p == NULL)
            break;
          s = p + 1;
        }
      }
      break;
    case 'I':
      add_system_inc_path(optarg);
      break;
    case 'D':
      define_macro(optarg);
      break;
    case 'L':
      vec_push(lib_paths, optarg);
      break;
    case OPT_WARNING:
      if (strcmp(optarg, "error") == 0) {
        error_warning = true;
      } else {
        // Silently ignored.
        // fprintf(stderr, "Warning: unknown option for -W: %s\n", optarg);
      }
      break;
    case OPT_NODEFAULTLIBS:
      nodefaultlibs = true;
      break;
    case OPT_NOSTDLIB:
      nostdlib = true;
      break;
    case OPT_STACK_SIZE:
      {
        int size = atoi(optarg);
        if (size <= 0) {
          error("stack-size must be positive");
        }
        stack_size = size;
      }
      break;
    case OPT_VERBOSE:
      verbose = true;
      break;
    case OPT_ENTRY_POINT:
      entry_point = *optarg != '\0' ? optarg : NULL;
      break;
    default:
      fprintf(stderr, "Warning: unknown option: %s\n", argv[optind - 1]);
      break;

    case OPT_OPTIMIZE:
    case OPT_DEBUGINFO:
    case OPT_ANSI:
    case OPT_STD:
    case OPT_PEDANTIC:
    case OPT_MMD:
      // Silently ignored.
      break;
    }
  }

  int iarg = optind;
  if (iarg >= argc) {
    fprintf(stderr, "No input files\n\n");
    // usage(stderr);
    return 1;
  }

  if (entry_point != NULL)
    vec_push(exports, alloc_name(entry_point, NULL, false));
  if (exports->len == 0) {
    error("no exports (require -e<xxx>)\n");
  }
  // Implicit export function.
  if (!nostdlib && !nodefaultlibs)
    vec_push(exports, alloc_name("sbrk", NULL, false));

  VERBOSES("### Exports\n");
  for (int i = 0; i < exports->len; ++i) {
    const Name *name = exports->data[i];
    VERBOSE("%.*s\n", name->bytes, name->chars);
  }
  VERBOSES("\n");

  Vector *sources = new_vector();
  for (int i = iarg; i < argc; ++i) {
    vec_push(sources, argv[i]);
  }
  // if (out_type >= OutExecutable)
  {
    vec_push(lib_paths, cat_path(root, "./lib"));
    if (!nostdlib)
      add_lib(lib_paths, "crt0.c", sources);
    if (!nodefaultlibs && !nostdlib)
      add_lib(lib_paths, "libc.c", sources);
  }

  // Preprocess.
  for (int i = 0; i < sources->len; ++i) {
    const char *filename = sources->data[i];
    FILE *ifp = fopen(filename, "r");
    if (ifp == NULL)
      error("Cannot open file: %s\n", filename);
    fprintf(ppout, "# 1 \"%s\" 1\n", filename);
    preprocess(ifp, filename);
    fclose(ifp);
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
  if (compile_error_count != 0)
    return 1;

  uint32_t address_bottom = traverse_ast(toplevel, exports, stack_size);
  if (compile_error_count != 0)
    return 1;

  gen(toplevel);
  if (compile_error_count != 0)
    return 1;
  if (error_warning && compile_warning_count != 0)
    return 2;

  FILE *fp = fopen(ofn, "wb");
  if (fp == NULL) {
    error("Cannot open output file");
  } else {
    emit_wasm(fp, exports, address_bottom);
    assert(compile_error_count == 0);
    fclose(fp);
  }

  return 0;
}
