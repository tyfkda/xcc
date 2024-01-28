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
#include "wasm.h"
#include "wasm_obj.h"

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

typedef struct {
  DataStorage *ds;
  Vector *reloc_data;
} EmitDataParam;

static void emit_fixnum(DataStorage *ds, Fixnum v, size_t size) {
  // Assume endian and CHAR_BIT are same on host and target.
  data_append(ds, (unsigned char*)&v, size);
}

static void emit_align(void *ud, int align) {
  EmitDataParam *edp = ud;
  DataStorage *ds = edp->ds;
  int offset = ds->len;
  int d = offset % align;
  if (d > 0) {
    d = align - d;
    for (int i = 0; i < d; ++i)
      data_push(ds, 0);
  }
}
static void emit_number(void *ud, const Type *type, Expr *var, Fixnum offset) {
  EmitDataParam *edp = ud;
  DataStorage *ds = edp->ds;
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

      if (out_type < OutExecutable) {
        RelocInfo *ri = calloc_or_die(sizeof(*ri));
        ri->type = R_WASM_MEMORY_ADDR_I32;
        ri->offset = ds->len;
        ri->addend = offset;
        ri->index = info->non_prim.symbol_index;

        Vector *reloc_data = edp->reloc_data;
        if (reloc_data == NULL)
          edp->reloc_data = reloc_data = new_vector();
        vec_push(reloc_data, ri);
      }
    }
  }
  emit_fixnum(ds, v, type_size(type));
}
static void emit_string(void *ud, Expr *str, size_t size) {
  assert(str->kind == EX_STR);
  EmitDataParam *edp = ud;
  DataStorage *ds = edp->ds;
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

static Vector *construct_data_segment_sub(DataStorage *ds, const VarInfo *varinfo) {
  static const ConstructInitialValueVTable kVtable = {
    .emit_align = emit_align,
    .emit_number = emit_number,
    .emit_string = emit_string,
  };
  EmitDataParam edp = {
    .ds = ds,
    .reloc_data = NULL,
  };
  construct_initial_value(varinfo->type, varinfo->global.init, &kVtable, &edp);
  return edp.reloc_data;
}

typedef struct {
  GVarInfo *gvarinfo;
  DataStorage ds;
  Vector *reloc_data;
} DataSegment;

static Vector *construct_data_segment(void) {
  // Enumerate global variables.
  Vector *segments = new_vector();
#ifndef NDEBUG
  uint32_t address = 0;
#endif
  for (int i = 0, len = global_scope->vars->len; i < len; ++i) {
    VarInfo *varinfo = global_scope->vars->data[i];
    if (varinfo->storage & VS_ENUM_MEMBER || varinfo->type->kind == TY_FUNC)
      continue;
    if (varinfo->global.init == NULL || !is_global_datsec_var(varinfo, global_scope))
      continue;
    GVarInfo *info = get_gvar_info_from_name(varinfo->name);
    if (info == NULL)
      continue;

#ifndef NDEBUG
    uint32_t adr = info->non_prim.address;
    assert(adr >= address);
#endif

    DataSegment *segment = malloc_or_die(sizeof(*segment));
    segment->gvarinfo = info;
    data_init(&segment->ds);
    segment->reloc_data = construct_data_segment_sub(&segment->ds, varinfo);
    vec_push(segments, segment);

#ifndef NDEBUG
    address = adr + type_size(varinfo->type);
#endif
  }
  return segments;
}

static int compare_indirect(const void *pa, const void *pb) {
  const FuncInfo *qa = *(const FuncInfo**)pa;
  const FuncInfo *qb = *(const FuncInfo**)pb;
  return (int)qa->indirect_index - (int)qb->indirect_index;
}

//

void emit_type_section(EmitWasm *ew) {
  DataStorage types_section;
  data_init(&types_section);
  data_open_chunk(&types_section);
  data_uleb128(&types_section, -1, functypes->len);  // num types
  for (int i = 0, len = functypes->len; i < len; ++i) {
    const DataStorage *wt = functypes->data[i];
    data_push(&types_section, WT_FUNC);  // func
    data_append(&types_section, wt->buf, wt->len);
  }
  data_close_chunk(&types_section, -1);

  fputc(SEC_TYPE, ew->ofp);
  fwrite(types_section.buf, types_section.len, 1, ew->ofp);
  ++ew->section_index;
}

static void emit_import_section(EmitWasm *ew) {
  static const char env_module_name[] = "env";

  DataStorage imports_section;
  data_init(&imports_section);
  data_open_chunk(&imports_section);
  data_open_chunk(&imports_section);
  uint32_t imports_count = 0;
  uint32_t global_count = 0;

  if (out_type < OutExecutable) {
    static const char kMemoryName[] = "__linear_memory";
    data_string(&imports_section, env_module_name, sizeof(env_module_name) - 1);  // import module name
    data_string(&imports_section, kMemoryName, sizeof(kMemoryName) - 1);  // import name
    data_push(&imports_section, IMPORT_MEMORY);  // import kind
    data_uleb128(&imports_section, -1, 0);  // index
    data_uleb128(&imports_section, -1, 0);  // size
    ++imports_count;
  }

  {
    const char *module_name = ew->import_module_name;
    size_t module_name_len = strlen(module_name);

    const Name *name;
    FuncInfo *info;
    for (int it = 0; (it = table_iterate(&func_info_table, it, &name, (void**)&info)) != -1; ) {
      if (info->flag == 0 || info->func != NULL)
        continue;
      VarInfo *varinfo = info->varinfo;
      if (varinfo->storage & VS_STATIC) {
        error("Import: `%.*s' is not public", NAMES(name));
      }

      data_string(&imports_section, module_name, module_name_len);  // import module name
      data_string(&imports_section, name->chars, name->bytes);  // import name
      data_push(&imports_section, IMPORT_FUNC);  // import kind
      data_uleb128(&imports_section, -1, info->type_index);  // import signature index
      ++imports_count;
    }
  }

  {
    const char *module_name = ew->import_module_name;
    size_t module_name_len = strlen(module_name);

    for (int i = 0, len = global_scope->vars->len; i < len; ++i) {
      VarInfo *varinfo = global_scope->vars->data[i];
      if (varinfo->storage & VS_ENUM_MEMBER || varinfo->type->kind == TY_FUNC)
        continue;
      if (is_global_datsec_var(varinfo, global_scope))
        continue;
      GVarInfo *info = get_gvar_info_from_name(varinfo->name);
      if (info == NULL || !(info->flag & GVF_UNRESOLVED))
        continue;

      const Name *name = varinfo->name;
      data_string(&imports_section, module_name, module_name_len);  // import module name
      data_string(&imports_section, name->chars, name->bytes);  // import name
      data_push(&imports_section, IMPORT_GLOBAL);  // import kind
      data_push(&imports_section, to_wtype(varinfo->type));  // type
      data_push(&imports_section, !(varinfo->type->qualifier & TQ_CONST));  // mutable
      ++imports_count;
      ++global_count;
    }
  }

  if (imports_count > 0) {
    data_close_chunk(&imports_section, imports_count);
    data_close_chunk(&imports_section, -1);

    fputc(SEC_IMPORT, ew->ofp);
    fwrite(imports_section.buf, imports_section.len, 1, ew->ofp);
    ++ew->section_index;
  }

  ew->import_global_count = global_count;
}

static void emit_function_section(EmitWasm *ew) {
  DataStorage functions_section;
  data_init(&functions_section);
  data_open_chunk(&functions_section);
  data_open_chunk(&functions_section);
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
  ew->function_count = function_count;
  if (function_count > 0) {
    data_close_chunk(&functions_section, function_count);  // num functions
    data_close_chunk(&functions_section, -1);

    fputc(SEC_FUNC, ew->ofp);
    fwrite(functions_section.buf, functions_section.len, 1, ew->ofp);
    ++ew->section_index;
  }
}

void emit_table_section(EmitWasm *ew) {
  if (out_type < OutExecutable)
    return;

  DataStorage table_section;
  data_init(&table_section);
  data_open_chunk(&table_section);
  data_leb128(&table_section, -1, 1);  // num tables
  data_push(&table_section, WT_FUNCREF);
  data_push(&table_section, 0x00);  // limits: flags
  data_leb128(&table_section, -1, ew->table_start_index + indirect_function_table.count);  // initial
  data_close_chunk(&table_section, -1);

  fputc(SEC_TABLE, ew->ofp);
  fwrite(table_section.buf, table_section.len, 1, ew->ofp);
  ++ew->section_index;
}

void emit_memory_section(EmitWasm *ew) {
  if (out_type < OutExecutable)
    return;

  DataStorage memory_section;
  data_init(&memory_section);
  data_open_chunk(&memory_section);
  data_open_chunk(&memory_section);
  {
    uint32_t page_count = (ew->address_bottom + MEMORY_PAGE_SIZE - 1) / MEMORY_PAGE_SIZE;
    if (page_count <= 0)
      page_count = 1;
    data_uleb128(&memory_section, -1, 0);  // index
    data_uleb128(&memory_section, -1, page_count);
    data_close_chunk(&memory_section, 1);  // count
    data_close_chunk(&memory_section, -1);
  }

  fputc(SEC_MEMORY, ew->ofp);
  fwrite(memory_section.buf, memory_section.len, 1, ew->ofp);
  ++ew->section_index;
}

static void emit_global_section(EmitWasm *ew) {
  DataStorage globals_section;
  data_init(&globals_section);
  data_open_chunk(&globals_section);
  data_open_chunk(&globals_section);
  uint32_t globals_count = 0;
  {
    for (int i = 0, len = global_scope->vars->len; i < len; ++i) {
      VarInfo *varinfo = global_scope->vars->data[i];
      if (varinfo->storage & VS_ENUM_MEMBER || varinfo->type->kind == TY_FUNC)
        continue;
      GVarInfo *info = get_gvar_info_from_name(varinfo->name);
      if (info == NULL)
        continue;

      if ((info->flag & GVF_UNRESOLVED) || is_global_datsec_var(varinfo, global_scope))
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
    data_close_chunk(&globals_section, globals_count);  // num globals
    data_close_chunk(&globals_section, -1);  // num globals

    fputc(SEC_GLOBAL, ew->ofp);
    fwrite(globals_section.buf, globals_section.len, 1, ew->ofp);
    ++ew->section_index;
  }
}

static void emit_export_section(EmitWasm *ew, Vector *exports) {
  if (out_type < OutExecutable)
    return;

  DataStorage exports_section;
  data_init(&exports_section);
  data_open_chunk(&exports_section);
  data_open_chunk(&exports_section);
  int num_exports = 0;
  for (int i = 0; i < exports->len; ++i) {
    const Name *name = exports->data[i];
    FuncInfo *info = table_get(&func_info_table, name);
    if (info == NULL) {
      error("Export: `%.*s' not found", NAMES(name));
    }
    assert(info->func != NULL);
    VarInfo *varinfo = info->varinfo;
    if (varinfo->storage & VS_STATIC) {
      error("Export: `%.*s' is not public", NAMES(name));
    }

    data_string(&exports_section, name->chars, name->bytes);  // export name
    data_uleb128(&exports_section, -1, IMPORT_FUNC);  // export kind
    data_uleb128(&exports_section, -1, info->index);  // export func index
    ++num_exports;
  }
  // Export globals.
  {
    const Name *name;
    GVarInfo *info;
    for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
      const VarInfo *varinfo = info->varinfo;
      if (!(info->flag & GVF_EXPORT) || !is_prim_type(varinfo->type) || (varinfo->storage & VS_REF_TAKEN))
        continue;
      data_string(&exports_section, name->chars, name->bytes);  // export name
      data_push(&exports_section, IMPORT_GLOBAL);  // export kind
      data_uleb128(&exports_section, -1, info->prim.index);  // export global index
      ++num_exports;
    }
  }
  /*if (memory_section.len > 0)*/ {  // TODO: Export only if memory exists
    static const char name[] = "memory";
    data_string(&exports_section, name, sizeof(name) - 1);  // export name
    data_uleb128(&exports_section, -1, IMPORT_MEMORY);  // export kind
    data_uleb128(&exports_section, -1, 0);  // export global index
    ++num_exports;
  }
  data_close_chunk(&exports_section, num_exports);  // num exports
  data_close_chunk(&exports_section, -1);

  fputc(SEC_EXPORT, ew->ofp);
  fwrite(exports_section.buf, exports_section.len, 1, ew->ofp);
  ++ew->section_index;
}

void emit_elems_section(EmitWasm *ew) {
  DataStorage elems_section;
  data_init(&elems_section);
  if (indirect_function_table.count > 0) {
    data_open_chunk(&elems_section);

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
    data_leb128(&elems_section, -1, ew->table_start_index);  // start index
    data_push(&elems_section, OP_END);
    data_leb128(&elems_section, -1, count);  // num elems
    for (int i = 0; i < count; ++i) {
      FuncInfo *info = indirect_funcs[i];
      VERBOSE("%2d: %.*s (%d)\n", i + 1, NAMES(info->func->name), (int)info->index);
      data_leb128(&elems_section, -1, info->index);  // elem function index
    }
    data_close_chunk(&elems_section, -1);
    VERBOSES("\n");

    fputc(SEC_ELEM, ew->ofp);
    fwrite(elems_section.buf, elems_section.len, 1, ew->ofp);
    ++ew->section_index;
  }
}

void emit_tag_section(EmitWasm *ew) {
  DataStorage tag_section;
  data_init(&tag_section);
  if (tags->len > 0) {
    data_open_chunk(&tag_section);
    data_uleb128(&tag_section, -1, tags->len);  // tag count
    for (int i = 0; i < tags->len; ++i) {
      int typeindex = VOIDP2INT(tags->data[i]);
      int attribute = 0;
      data_uleb128(&tag_section, -1, attribute);
      data_uleb128(&tag_section, -1, typeindex);
    }
    data_close_chunk(&tag_section, -1);

    fputc(SEC_TAG, ew->ofp);
    fwrite(tag_section.buf, tag_section.len, 1, ew->ofp);
    ++ew->section_index;
  }
}

static void emit_code_section(EmitWasm *ew) {
  if (ew->function_count <= 0)
    return;

  DataStorage codesec;
  data_init(&codesec);
  data_open_chunk(&codesec);
  data_uleb128(&codesec, -1, ew->function_count);  // num functions
  {
    const Name *name;
    FuncInfo *info;
    size_t offset = codesec.len;
    for (int it = 0; (it = table_iterate(&func_info_table, it, &name, (void**)&info)) != -1; ) {
      Function *func = info->func;
      if (func == NULL || info->flag == 0)
        continue;
      FuncExtra* extra = func->extra;
      DataStorage *code = extra->code;
      data_concat(&codesec, code);

      extra->offset += offset;
      offset += code->len;
    }
  }
  data_close_chunk(&codesec, -1);

  fputc(SEC_CODE, ew->ofp);
  fwrite(codesec.buf, codesec.len, 1, ew->ofp);
  ++ew->section_index;
}

static Vector *emit_data_section(EmitWasm *ew) {
  Vector *segments = construct_data_segment();
  Vector *reloc_data = NULL;
  if (segments->len > 0) {
    reloc_data = new_vector();

    VERBOSES("### Data\n");
    DataStorage datasec;
    data_init(&datasec);
    data_open_chunk(&datasec);
    data_uleb128(&datasec, -1, segments->len);
    for (int i = 0; i < segments->len; ++i) {
      DataSegment *segment = segments->data[i];
      data_push(&datasec, 0);  // flags
      // Init (address).
      uint32_t address = segment->gvarinfo->non_prim.address;
      VERBOSE("%04x: %.*s (size=%zu)\n", address, NAMES(segment->gvarinfo->varinfo->name),
              type_size(segment->gvarinfo->varinfo->type));
      data_push(&datasec, OP_I32_CONST);
      data_leb128(&datasec, -1, address);
      data_push(&datasec, OP_END);
      // Content
      data_uleb128(&datasec, -1, segment->ds.len);
      uint32_t offset = datasec.len;
      data_concat(&datasec, &segment->ds);

      // Reloc data.
      Vector *relocs = segment->reloc_data;
      if (relocs != NULL) {
        for (int i = 0; i < relocs->len; ++i) {
          RelocInfo *reloc = relocs->data[i];
          reloc->offset += offset;
          vec_push(reloc_data, reloc);
        }
      }
    }
    data_close_chunk(&datasec, -1);
    VERBOSES("\n");

    fputc(SEC_DATA, ew->ofp);
    fwrite(datasec.buf, datasec.len, 1, ew->ofp);
    ++ew->section_index;
  }
  return reloc_data;
}

static void emit_linking_section(EmitWasm *ew) {
  DataStorage linking_section;
  static const char kLinkingName[] = "linking";
  const int LINK_VERSION = 2;
  data_init(&linking_section);
  data_open_chunk(&linking_section);
  data_string(&linking_section, kLinkingName, sizeof(kLinkingName) - 1);
  data_uleb128(&linking_section, -1, LINK_VERSION);
  data_push(&linking_section, LT_WASM_SYMBOL_TABLE);  // subsec type

  data_open_chunk(&linking_section);  // Payload start.
  data_open_chunk(&linking_section);
  uint32_t count = 0;
  // Functions
  for (int k = 0; k < 2; ++k) {  // To match function index and linking order, do twice.
    const Name *name;
    FuncInfo *info;
    for (int it = 0; (it = table_iterate(&func_info_table, it, &name, (void**)&info)) != -1; ) {
      if (info->flag == 0 ||
          (k == 0 && info->func != NULL) ||  // Put external function first.
          (k == 1 && info->func == NULL))    // Defined function later.
        continue;

      int flags = 0;
      if (info->func == NULL)
        flags |= WASM_SYM_UNDEFINED;
      if (info->varinfo->storage & VS_STATIC)
        flags |= WASM_SYM_VISIBILITY_HIDDEN;

      data_push(&linking_section, SIK_SYMTAB_FUNCTION);  // kind
      data_uleb128(&linking_section, -1, flags);
      data_uleb128(&linking_section, -1, info->index);

      if (info->func != NULL) {  // Defined function: put name. otherwise not required.
        data_string(&linking_section, name->chars, name->bytes);
      }
      ++count;
    }
  }
  // Globals
  for (int k = 0; k < 2; ++k) {  // 0=unresolved, 1=resolved
    for (int i = 0, len = global_scope->vars->len; i < len; ++i) {
      VarInfo *varinfo = global_scope->vars->data[i];
      if (varinfo->storage & VS_ENUM_MEMBER || varinfo->type->kind == TY_FUNC)
        continue;
      GVarInfo *info = get_gvar_info_from_name(varinfo->name);
      if (info == NULL)
        continue;
      if ((k == 0 && !(info->flag & GVF_UNRESOLVED)) || (k != 0 && (info->flag & GVF_UNRESOLVED)))
        continue;

      int flags = 0;
      if (info->flag & GVF_UNRESOLVED)
        flags |= WASM_SYM_UNDEFINED;
      if (varinfo->storage & VS_STATIC)
        flags |= WASM_SYM_VISIBILITY_HIDDEN;

      if (is_global_datsec_var(varinfo, global_scope)) {
        data_push(&linking_section, SIK_SYMTAB_DATA);  // kind
        data_uleb128(&linking_section, -1, flags);
        const Name *name = varinfo->name;
        data_string(&linking_section, name->chars, name->bytes);
        if (!(info->flag & GVF_UNRESOLVED)) {  // Defined global: put name. otherwise not required.
          data_uleb128(&linking_section, -1, info->non_prim.item_index);
          data_uleb128(&linking_section, -1, 0);  // offset (must start from the begining)
          data_uleb128(&linking_section, -1, type_size(varinfo->type));  // size
        }
      } else {
        data_push(&linking_section, SIK_SYMTAB_GLOBAL);  // kind
        data_uleb128(&linking_section, -1, flags);
        data_uleb128(&linking_section, -1, info->non_prim.item_index);
        // if (!(info->flag & GVF_UNRESOLVED)) {
        if (info->non_prim.item_index >= ew->import_global_count) {
          const Name *name = varinfo->name;
          data_string(&linking_section, name->chars, name->bytes);
        }
      }

      ++count;
    }
  }
  data_close_chunk(&linking_section, count);
  data_close_chunk(&linking_section, -1);  // Put payload size.

  if (linking_section.len > 0) {
    data_close_chunk(&linking_section, -1);

    fputc(SEC_CUSTOM, ew->ofp);
    fwrite(linking_section.buf, linking_section.len, 1, ew->ofp);
    ++ew->section_index;
  }
}

static void emit_reloc_section(EmitWasm *ew, int section_index, Vector *relocs, const char *name) {
  assert(relocs != NULL);
  if (relocs->len > 0) {
    DataStorage ds;
    data_init(&ds);
    data_open_chunk(&ds);
    data_string(&ds, name, strlen(name));
    data_uleb128(&ds, -1, section_index);

    int count = relocs->len;
    data_uleb128(&ds, -1, count);
    for (int i = 0; i < count; ++i) {
      RelocInfo *reloc = relocs->data[i];
      data_push(&ds, reloc->type);
      data_uleb128(&ds, -1, reloc->offset);
      data_uleb128(&ds, -1, reloc->index);
      switch (reloc->type) {
      case R_WASM_MEMORY_ADDR_LEB:
      case R_WASM_MEMORY_ADDR_SLEB:
      case R_WASM_MEMORY_ADDR_I32:
      case R_WASM_MEMORY_ADDR_LEB64:
      case R_WASM_MEMORY_ADDR_SLEB64:
      case R_WASM_MEMORY_ADDR_I64:
      case R_WASM_FUNCTION_OFFSET_I32:
      case R_WASM_SECTION_OFFSET_I32:
        data_uleb128(&ds, -1, reloc->addend);
        break;
      default: break;
      }
      // TODO: addend?
    }
    data_close_chunk(&ds, -1);

    fputc(SEC_CUSTOM, ew->ofp);
    fwrite(ds.buf, ds.len, 1, ew->ofp);
    ++ew->section_index;
  }
}

static void emit_reloc_code_section(EmitWasm *ew) {
  typedef struct {
    Function *func;
    RelocInfo *reloc;
  } CodeReloc;

  Vector *code_reloc_all = new_vector();

  const Name *name;
  FuncInfo *info;
  for (int it = 0; (it = table_iterate(&func_info_table, it, &name, (void**)&info)) != -1; ) {
    Function *func = info->func;
    if (func == NULL || info->flag == 0)
      continue;

    FuncExtra *extra = func->extra;
    Vector *reloc_code = extra->reloc_code;
    for (int i = 0; i < reloc_code->len; ++i) {
      CodeReloc *cr = calloc_or_die(sizeof(*cr));
      cr->func = func;
      cr->reloc = reloc_code->data[i];
      vec_push(code_reloc_all, cr);
    }
  }

  int count = code_reloc_all->len;
  if (count > 0) {
    for (int i = 0; i < count; ++i) {
      CodeReloc *cr = code_reloc_all->data[i];
      Function *func = cr->func;
      RelocInfo *reloc = cr->reloc;
      FuncExtra *extra = func->extra;
      reloc->offset += extra->offset;
      code_reloc_all->data[i] = reloc;  // Replace from CodeReloc* to RelocInfo*
    }

    static const char kRelocCode[] = "reloc.CODE";
    emit_reloc_section(ew, ew->code_section_index, code_reloc_all, kRelocCode);
  }
}

static void emit_reloc_data_section(EmitWasm *ew, Vector *reloc_data) {
  if (reloc_data == NULL)
    return;

  static const char kRelocData[] = "reloc.DATA";
  emit_reloc_section(ew, ew->data_section_index, reloc_data, kRelocData);
}

void emit_wasm(FILE *ofp, Vector *exports, const char *import_module_name,
               uint32_t address_bottom) {
  write_wasm_header(ofp);

  EmitWasm ew_body = {
    .ofp = ofp,
    .import_module_name = import_module_name,
    .address_bottom = address_bottom,
    .table_start_index = 1,  // TODO: Output table only when it is needed.
  };
  EmitWasm *ew = &ew_body;

  // Types.
  emit_type_section(ew);

  // Imports.
  emit_import_section(ew);

  // Functions.
  emit_function_section(ew);

  // Table.
  emit_table_section(ew);

  // Memory.
  emit_memory_section(ew);

  // Tag (must put earlier than Global section.)
  emit_tag_section(ew);

  // Globals.
  emit_global_section(ew);

  // Exports.
  emit_export_section(ew, exports);

  // Elements.
  emit_elems_section(ew);

  // Code.
  ew->code_section_index = ew->section_index;
  emit_code_section(ew);

  // Data.
  ew->data_section_index = ew->section_index;
  Vector *reloc_data = emit_data_section(ew);

  if (out_type < OutExecutable) {
    emit_linking_section(ew);
    emit_reloc_code_section(ew);
    emit_reloc_data_section(ew, reloc_data);
  }
}
