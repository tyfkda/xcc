#include "../config.h"
#include "wcc.h"

#include <assert.h>
#include <stdlib.h>  // free
#include <string.h>

#include "ast.h"
#include "cc_misc.h"
#include "fe_misc.h"
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
          assert(info != NULL);
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
        data_append(ds, &f, sizeof(float));  // TODO: Endian
      } else {
        data_push(ds, OP_F64_CONST);
        uint64_t d = offset;
        data_append(ds, &d, sizeof(double));  // TODO: Endian
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
  data_append(ds, &v, size);
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

      FuncInfo *info = table_get(&func_info_table, var->var.name);
      if (info == NULL)
        error("`%.*s' not found", NAMES(var->var.name));

      RelocInfo *ri = calloc_or_die(sizeof(*ri));
      ri->type = R_WASM_TABLE_INDEX_I32;
      ri->offset = ds->len;
      ri->addend = offset;
      ri->index = info->index;

      Vector *reloc_data = edp->reloc_data;
      if (reloc_data == NULL)
        edp->reloc_data = reloc_data = new_vector();
      vec_push(reloc_data, ri);
    } else {
      assert(var->kind == EX_VAR);
      const GVarInfo *info = get_gvar_info(var);
      assert(info != NULL);
      assert(!is_prim_type(info->varinfo->type) || (info->varinfo->storage & VS_REF_TAKEN));
      v += info->non_prim.address;

      RelocInfo *ri = calloc_or_die(sizeof(*ri));
      ri->type = R_WASM_MEMORY_ADDR_I32;
      ri->offset = ds->len;
      ri->addend = offset;
      ri->index = info->symbol_index;

      Vector *reloc_data = edp->reloc_data;
      if (reloc_data == NULL)
        edp->reloc_data = reloc_data = new_vector();
      vec_push(reloc_data, ri);
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
    data_append(ds, str->str.buf, size);
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
  uint32_t p2align;
} DataSegment;

static Vector *construct_data_segment(void) {  // <DataSegment*>
  // Enumerate global variables.
  Vector *segments = new_vector();
#ifndef NDEBUG
  uint32_t address = 0;
#endif
  for (int k = 0; k < 2; ++k) {  // 0=data, 1=bss
    const Name *name;
    GVarInfo *info;
    for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
      const VarInfo *varinfo = info->varinfo;
      int storage = varinfo->storage;
      if (varinfo->type->kind == TY_FUNC ||
          (storage & (VS_EXTERN | VS_ENUM_MEMBER)) ||
          (storage & (VS_STATIC | VS_USED)) == VS_STATIC)  // Static variable but not used.
        continue;
      if ((k == 0) == (varinfo->global.init == NULL) ||
          !is_global_datsec_var(varinfo, global_scope))
        continue;

#ifndef NDEBUG
      uint32_t adr = info->non_prim.address;
      assert(adr >= address);
#endif

      DataSegment *segment = calloc_or_die(sizeof(*segment));
      segment->gvarinfo = info;
      size_t align = align_size(varinfo->type);
      uint32_t p2align;
      for (p2align = 0; align > (1U << p2align); ++p2align)
        ;
      segment->p2align = p2align;
      data_init(&segment->ds);
      if (k == 0) {
        segment->reloc_data = construct_data_segment_sub(&segment->ds, varinfo);
      } else {
        // Output BSS as zero-filled data to tell its size.
        DataStorage *ds = &segment->ds;
        size_t start = ds->len;
        size_t size = type_size(varinfo->type);
        data_reserve(ds, start + size);
        memset(ds->buf + start, 0, size);
        ds->len += size;
      }
      vec_push(segments, segment);

#ifndef NDEBUG
      address = adr + type_size(varinfo->type);
#endif
    }
  }
  return segments;
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

  if (compile_unit_flag & (CUF_LINEAR_MEMORY | CUF_USE_SP)) {
    static const char kMemoryName[] = "__linear_memory";
    data_string(&imports_section, env_module_name, sizeof(env_module_name) - 1);  // import module name
    data_string(&imports_section, kMemoryName, sizeof(kMemoryName) - 1);  // import name
    data_push(&imports_section, IMPORT_MEMORY);  // import kind
    data_uleb128(&imports_section, -1, 0);  // index
    data_uleb128(&imports_section, -1, 0);  // size
    ++imports_count;
  }
  if (tables->len > 0) {
    for (int i = 0; i < tables->len; ++i) {
      TableInfo *t = tables->data[i];
      data_string(&imports_section, env_module_name, sizeof(env_module_name) - 1);  // import module name
      data_string(&imports_section, t->name->chars, t->name->bytes);  // import name
      data_push(&imports_section, IMPORT_TABLE);  // import kind
      data_push(&imports_section, t->type);
      data_push(&imports_section, 0x00);  // limits: flags
      data_leb128(&imports_section, -1, INDIRECT_FUNCTION_TABLE_START_INDEX);  // initial
      ++imports_count;
    }
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

      const char *modname = module_name;
      size_t modnamelen = module_name_len;
      if (info->module_name != NULL) {
        modname = info->module_name->chars;
        modnamelen = info->module_name->bytes;
      }
      const Name *fn = info->func_name;

      data_string(&imports_section, modname, modnamelen);  // import module name
      data_string(&imports_section, fn->chars, fn->bytes);  // import name
      data_push(&imports_section, IMPORT_FUNC);  // import kind
      data_uleb128(&imports_section, -1, info->type_index);  // import signature index
      ++imports_count;
    }
  }

  {
    const char *module_name = ew->import_module_name;
    size_t module_name_len = strlen(module_name);

    const Name *name;
    GVarInfo *info;
    for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
      if (!(info->flag & GVF_UNRESOLVED))
        continue;
      VarInfo *varinfo = info->varinfo;
      if (varinfo->storage & VS_ENUM_MEMBER || varinfo->type->kind == TY_FUNC ||
          is_global_datsec_var(varinfo, global_scope))
        continue;

      const Name *name = varinfo->ident->ident;
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
      if (info->func == NULL || is_function_omitted(info->varinfo))
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

static void emit_global_section(EmitWasm *ew) {
  DataStorage globals_section;
  data_init(&globals_section);
  data_open_chunk(&globals_section);
  data_open_chunk(&globals_section);
  uint32_t globals_count = 0;
  for (int k = 0; k < 2; ++k) {  // 0=resolved(data), 1=resolved(bss)
    const Name *name;
    GVarInfo *info;
    for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
      const VarInfo *varinfo = info->varinfo;
      if (varinfo->storage & VS_ENUM_MEMBER || varinfo->type->kind == TY_FUNC)
        continue;

      if ((info->flag & GVF_UNRESOLVED) || is_global_datsec_var(varinfo, global_scope) ||
          (varinfo->global.init == NULL) == (k == 0))
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

static void emit_export_section(EmitWasm *ew) {
  Table *exports = ew->exports;
  if (exports == NULL || exports->count <= 0)
    return;

  DataStorage exports_section;
  data_init(&exports_section);
  data_open_chunk(&exports_section);
  data_open_chunk(&exports_section);

  const Name *name;
  for (int it = 0; (it = table_iterate(exports, it, &name, NULL)) != -1; ) {
    FuncInfo *info = table_get(&func_info_table, name);
    assert(info != NULL);

    data_string(&exports_section, name->chars, name->bytes);  // export name
    data_uleb128(&exports_section, -1, IMPORT_FUNC);  // export kind
    data_uleb128(&exports_section, -1, info->index);  // export func index
  }
  data_close_chunk(&exports_section, exports->count);  // num exports
  data_close_chunk(&exports_section, -1);

  fputc(SEC_EXPORT, ew->ofp);
  fwrite(exports_section.buf, exports_section.len, 1, ew->ofp);
}

static void emit_elems_section(EmitWasm *ew) {
  int count = indirect_function_table.count;
  if (count == 0)
    return;

  DataStorage elems_section;
  data_init(&elems_section);
  data_open_chunk(&elems_section);

  // Enumerate imported functions.
  VERBOSES("### Indirect functions\n");
  data_leb128(&elems_section, -1, 1);  // num elem segments
  data_leb128(&elems_section, -1, 0);  // segment flags
  data_push(&elems_section, OP_I32_CONST);
  data_leb128(&elems_section, -1, INDIRECT_FUNCTION_TABLE_START_INDEX);  // start index
  data_push(&elems_section, OP_END);
  data_leb128(&elems_section, -1, count);  // num elems
  const Name *name;
  FuncInfo *info;
  for (int it = 0;
        (it = table_iterate(&indirect_function_table, it, &name, (void**)&info)) != -1; ) {
    VERBOSE("%.*s (%u)\n", NAMES(info->varinfo->ident->ident), info->index);
    data_leb128(&elems_section, -1, info->index);  // elem function index
  }
  data_close_chunk(&elems_section, -1);
  VERBOSES("\n");

  fputc(SEC_ELEM, ew->ofp);
  fwrite(elems_section.buf, elems_section.len, 1, ew->ofp);
  ++ew->section_index;
}

void emit_tag_section(EmitWasm *ew) {
  DataStorage tag_section;
  data_init(&tag_section);
  if (tags->len > 0) {
    data_open_chunk(&tag_section);
    data_uleb128(&tag_section, -1, tags->len);  // tag count
    for (int i = 0; i < tags->len; ++i) {
      TagInfo *t = tags->data[i];
      int attribute = 0;
      data_uleb128(&tag_section, -1, attribute);
      data_uleb128(&tag_section, -1, t->typeindex);
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
      if (func == NULL || is_function_omitted(info->varinfo))
        continue;

      FuncExtra *extra = func->extra;
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
  Vector *segments = construct_data_segment();  // <DataSegment*>
  ew->data_segments = segments;
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
      VERBOSE("%04x: %.*s  (size=0x%zx)\n", address,
              NAMES(segment->gvarinfo->varinfo->ident->ident),
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

static void emit_linking_section(EmitWasm *ew, Vector *decls) {
  DataStorage linking_section;
  static const char kLinkingName[] = "linking";
  const int LINK_VERSION = 2;
  data_init(&linking_section);
  data_open_chunk(&linking_section);
  data_string(&linking_section, kLinkingName, sizeof(kLinkingName) - 1);
  data_uleb128(&linking_section, -1, LINK_VERSION);

  // Symbol table.
  data_push(&linking_section, LT_WASM_SYMBOL_TABLE);  // subsec type
  data_open_chunk(&linking_section);  // Payload start.
  data_open_chunk(&linking_section);
  uint32_t count = 0;
  // Functions
  for (int k = 0; k < 2; ++k) {  // To match function index and linking order, do twice.
    const Name *name;
    FuncInfo *info;
    for (int it = 0; (it = table_iterate(&func_info_table, it, &name, (void**)&info)) != -1; ) {
      if ((k == 0 && (info->func != NULL || info->flag == 0)) ||  // Put external function first.
          (k == 1 && info->func == NULL))                         // Defined function later.
        continue;
      if (is_function_omitted(info->varinfo))
        continue;

      int flags = 0;
      if (info->func == NULL)
        flags |= WASM_SYM_UNDEFINED;
      if (info->varinfo->storage & VS_STATIC)
        flags |= WASM_SYM_BINDING_LOCAL | WASM_SYM_VISIBILITY_HIDDEN;
      if (info->flag & FF_WEAK)
        flags |= WASM_SYM_BINDING_WEAK;
      if (info->flag & FF_IMPORT_NAME) {
        // __attribute((import_name("..."))) is specified:
        flags |= WASM_SYM_EXPLICIT_NAME;
      }

      data_push(&linking_section, SIK_SYMTAB_FUNCTION);  // kind
      data_uleb128(&linking_section, -1, flags);
      data_uleb128(&linking_section, -1, info->index);

      if (info->func != NULL ||  // Defined function: put name. otherwise not required.
          flags & WASM_SYM_EXPLICIT_NAME) {
        data_string(&linking_section, name->chars, name->bytes);
      }
      ++count;
    }
  }
  // Globals
  for (int k = 0; k < 3; ++k) {  // 0=unresolved, 1=resolved(data), 2=resolved(bss)
    const Name *name;
    GVarInfo *info;
    int flags_bss = k != 2 ? 0 : cc_flags.common ? WASM_SYM_BINDING_WEAK : 0;
    for (int it = 0; (it = table_iterate(&gvar_info_table, it, &name, (void**)&info)) != -1; ) {
      const VarInfo *varinfo = info->varinfo;
      int storage = varinfo->storage;
      if (varinfo->type->kind == TY_FUNC ||
          (storage & VS_ENUM_MEMBER) ||  // VS_EXPORT is not set because of `__stack_pointer`.
          (storage & (VS_STATIC | VS_USED)) == VS_STATIC)  // Static variable but not used.
        continue;
      GVarInfo *info = get_gvar_info_from_name(varinfo->ident->ident);
      if (info == NULL)
        continue;
      if ((k == 0 && !(info->flag & GVF_UNRESOLVED)) ||
          (k != 0 && ((info->flag & GVF_UNRESOLVED) || (varinfo->global.init == NULL) == (k == 1))))
        continue;

      int flags = flags_bss;
      if (info->flag & GVF_UNRESOLVED)
        flags |= WASM_SYM_UNDEFINED;
      if (varinfo->storage & VS_STATIC)
        flags |= WASM_SYM_BINDING_LOCAL | WASM_SYM_VISIBILITY_HIDDEN;

      if (is_global_datsec_var(varinfo, global_scope)) {
        data_push(&linking_section, SIK_SYMTAB_DATA);  // kind
        data_uleb128(&linking_section, -1, flags);
        const Name *name = varinfo->ident->ident;
        data_string(&linking_section, name->chars, name->bytes);
        if (!(info->flag & GVF_UNRESOLVED)) {  // Defined global: put name. otherwise not required.
          data_uleb128(&linking_section, -1, info->item_index);
          data_uleb128(&linking_section, -1, 0);  // offset (must start from the begining)
          data_uleb128(&linking_section, -1, type_size(varinfo->type));  // size
        }
      } else {
        data_push(&linking_section, SIK_SYMTAB_GLOBAL);  // kind
        data_uleb128(&linking_section, -1, flags);
        data_uleb128(&linking_section, -1, info->item_index);
        if (info->item_index >= ew->import_global_count) {
          const Name *name = varinfo->ident->ident;
          data_string(&linking_section, name->chars, name->bytes);
        }
      }

      ++count;
    }
  }
  if (tables->len > 0) {  // Table
    for (int i = 0, len = tables->len; i < len; ++i) {
      TableInfo *ti = tables->data[i];
      int flags = WASM_SYM_UNDEFINED | WASM_SYM_EXPORTED | WASM_SYM_NO_STRIP;
      data_push(&linking_section, SIK_SYMTAB_TABLE);  // kind
      data_uleb128(&linking_section, -1, flags);
      data_uleb128(&linking_section, -1, ti->index);
      ++count;
    }
  }
  if (tags->len > 0) {  // Tag
    for (int i = 0, len = tags->len; i < len; ++i) {
      TagInfo *ti = tags->data[i];
      int flags = WASM_SYM_BINDING_WEAK;
      data_push(&linking_section, SIK_SYMTAB_EVENT);  // kind
      data_uleb128(&linking_section, -1, flags);
      data_uleb128(&linking_section, -1, ti->index);
      data_string(&linking_section, ti->name->chars, ti->name->bytes);
      ++count;
    }
  }
  data_close_chunk(&linking_section, count);
  data_close_chunk(&linking_section, -1);  // Put payload size.

  // Data segments.
  Vector *segments = ew->data_segments;
  if (segments->len > 0) {
    data_push(&linking_section, LT_WASM_SEGMENT_INFO);  // subsec type
    data_open_chunk(&linking_section);  // Payload start.
    data_uleb128(&linking_section, -1, segments->len);
    for (int i = 0; i < segments->len; ++i) {
      DataSegment *segment = segments->data[i];
      VarInfo *varinfo = segment->gvarinfo->varinfo;
      int flags = 0;
      if (varinfo->global.init != NULL && varinfo->global.init->kind == IK_SINGLE) {
        Expr *e = varinfo->global.init->single;
        if (e->kind == EX_STR && e->str.kind == STR_CHAR) {
          const Type *type = varinfo->type;
          if (type->kind == TY_ARRAY && type->pa.length > 0 && (varinfo->storage & VS_STRING) &&
              type->pa.length == (ssize_t)e->str.len && e->str.buf[type->pa.length - 1] == '\0')
            flags |= WASM_SEG_FLAG_STRINGS;
        }
      }

      const Name *name = varinfo->ident->ident;
      data_string(&linking_section, name->chars, name->bytes);
      data_uleb128(&linking_section, -1, segment->p2align);
      data_uleb128(&linking_section, -1, flags);
    }
    data_close_chunk(&linking_section, -1);
  }

  {
    AttrFuncContainer ctors;
    enumerate_ctor_dtors(decls, &ctors, NULL);
    if (ctors.len > 0) {
      data_push(&linking_section, LT_WASM_INIT_FUNCS);  // subsec type
      data_open_chunk(&linking_section);  // Payload start.
      data_uleb128(&linking_section, -1, ctors.len);  // Count
      for (int i = 0; i < ctors.len; ++i) {
        Function *func = ctors.data[i].func;
        FuncInfo *info;
        info = table_get(&func_info_table, func->ident->ident);
        assert(info != NULL);
        data_uleb128(&linking_section, -1, 65535);  // Priority
        data_uleb128(&linking_section, -1, info->index);  // Symbol index
      }
      data_close_chunk(&linking_section, -1);
    }
  }

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
        data_leb128(&ds, -1, reloc->addend);
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
    if (func == NULL || is_function_omitted(info->varinfo))
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

void emit_wasm(FILE *ofp, const char *import_module_name, Table *exports, Vector *decls) {
  write_wasm_header(ofp);

  EmitWasm ew_body = {
    .ofp = ofp,
    .import_module_name = import_module_name,
    .exports = exports,
  };
  EmitWasm *ew = &ew_body;

  // Types.
  emit_type_section(ew);

  // Imports.
  emit_import_section(ew);

  // Functions.
  emit_function_section(ew);

  // Tag (must put earlier than Global section.)
  emit_tag_section(ew);

  // Globals.
  emit_global_section(ew);

  // Exports.
  emit_export_section(ew);

  // Elements.
  emit_elems_section(ew);

  // Code.
  ew->code_section_index = ew->section_index;
  emit_code_section(ew);

  // Data.
  ew->data_section_index = ew->section_index;
  Vector *reloc_data = emit_data_section(ew);

  // Linking.
  emit_linking_section(ew, decls);
  emit_reloc_code_section(ew);
  emit_reloc_data_section(ew, reloc_data);
}
