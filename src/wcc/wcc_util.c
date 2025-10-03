#include "wcc.h"

#include <assert.h>
#include <stdlib.h>  // realloc, free
#include <string.h>

#include "ast.h"
#include "fe_misc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "wasm.h"

const char SP_NAME[] = "__stack_pointer";  // Variable name for stack pointer (global).
const char HEAP_BASE_NAME[] = "__heap_base";
const char INDIRECT_FUNCALL_TABLE_NAME[] = "__indirect_function_table";

bool verbose;
Table func_info_table;
Table gvar_info_table;
Vector *functypes;  // <DataStorage*>
Table indirect_function_table;
Vector *tags;
Vector *tables;
Vector *init_funcs;
int compile_unit_flag;

uint32_t get_indirect_function_index(const Name *name) {
  FuncInfo *info = table_get(&indirect_function_table, name);
  assert(info != NULL && info->indirect_index > 0);
  return info->indirect_index;
}

GVarInfo *register_gvar_info(const Name *name, VarInfo *varinfo) {
#if !defined(NDEBUG)
  assert(!table_try_get(&gvar_info_table, name, NULL));
#endif
  GVarInfo *info = calloc_or_die(sizeof(*info));
  info->varinfo = varinfo;
  info->flag = 0;
  table_put(&gvar_info_table, name, info);
  return info;
}

GVarInfo *get_gvar_info_from_name(const Name *name) {
  return table_get(&gvar_info_table, name);
}

int getsert_func_type(unsigned char *buf, size_t size, bool reg) {
  for (int i = 0, len = functypes->len; i < len; ++i) {
    const DataStorage *t = functypes->data[i];
    if (t->len == size && memcmp(t->buf, buf, size) == 0)
      return i;
  }
  if (reg) {
    int index = functypes->len;
    DataStorage *p = malloc_or_die(sizeof(*p));
    p->buf = buf;  // Warning: `buf` is moved!
    p->len = size;
    vec_push(functypes, p);
    return index;
  }
  return -1;
}

TagInfo *getsert_tag(const Name *name, int typeindex) {
  uint32_t len = tags->len;
  for (uint32_t i = 0; i < len; ++i) {
    TagInfo *ti = tags->data[i];
    if (equal_name(ti->name, name)) {
      if (ti->typeindex != typeindex)
        error("Tag type mismatch: %.*s", NAMES(name));
      return ti;
    }
  }

  TagInfo *ti = calloc_or_die(sizeof(*ti));
  ti->name = name;
  ti->typeindex = typeindex;
  ti->index = len;
  vec_push(tags, ti);
  return ti;
}

TableInfo *getsert_table(const Name *name, int type) {
  uint32_t len = tables->len;
  for (uint32_t i = 0; i < len; ++i) {
    TableInfo *ti = tables->data[i];
    if (equal_name(ti->name, name)) {
      if (ti->type != type)
        error("Table type mismatch: %.*s", NAMES(name));
      return ti;
    }
  }

  TableInfo *ti = calloc_or_die(sizeof(*ti));
  ti->name = name;
  ti->type = type;
  ti->index = len;
  vec_push(tables, ti);
  return ti;
}

TableInfo *getsert_indirect_function_table(void) {
  return getsert_table(alloc_name(INDIRECT_FUNCALL_TABLE_NAME, NULL, false), WT_FUNCREF);
}

//

void write_wasm_header(FILE *ofp) {
  WasmHeader header = {
    WASM_BINARY_MAGIC,
    WASM_BINARY_VERSION,
  };
  fwrite(&header, sizeof(header), 1, ofp);
}

//

Expr *get_sp_var(void) {
  const Name *spname = alloc_name(SP_NAME, NULL, false);
  GVarInfo *info = get_gvar_info_from_name(spname);
  assert(info != NULL);
  return new_expr_variable(spname, info->varinfo->type, NULL, global_scope);
}

unsigned char to_wtype(const Type *type) {
  switch (type->kind) {
  case TY_FIXNUM: return type_size(type) <= I32_SIZE ? WT_I32 : WT_I64;
  case TY_FLONUM: return type->flonum.kind < FL_DOUBLE ? WT_F32 : WT_F64;
  case TY_PTR:
  case TY_ARRAY:
    // Pointer and array is handled as an index of linear memroy.
    return WT_I32;
  default: assert(!"Illegal"); break;
  }
  return WT_I32;
}

bool is_global_datsec_var(const VarInfo *varinfo, Scope *scope) {
#define PUT_GLOBAL_ON_DATA_SECTION  (1)

  if (!is_global_scope(scope) && is_local_storage(varinfo))
    return false;
#if !PUT_GLOBAL_ON_DATA_SECTION
  if (is_prim_type(varinfo->type) && !(varinfo->storage & VS_REF_TAKEN))
    return false;
#else
  // Special: Stack pointer and break address.
  if (equal_name(varinfo->ident->ident, alloc_name(SP_NAME, NULL, false)))
    return false;
#endif
  return true;
}

size_t calc_funcall_work_size(Expr *expr) {
  assert(expr->kind == EX_FUNCALL);
  Expr *func = expr->funcall.func;
  Type *functype = get_callee_type(func->type);
  assert(functype != NULL);
  int param_count = functype->func.params != NULL ? functype->func.params->len : 0;
  Vector *args = expr->funcall.args;

  size_t work_size = 0;
  for (int i = 0; i < param_count; ++i) {
    Expr *arg = args->data[i];
    const Type *type = arg->type;
    if (is_stack_param(type) && !is_small_struct(type))
      work_size = ALIGN(work_size, align_size(type)) + type_size(type);
  }

  if (functype->func.vaargs) {
    int arg_count = args->len;
    int d = arg_count - param_count;
    if (d > 0) {
      for (int i = 0; i < d; ++i) {
        Expr *arg = args->data[i + param_count];
        const Type *t = arg->type;
        assert(!(t->kind == TY_FIXNUM && t->fixnum.kind < FX_INT));
        work_size = ALIGN(work_size, align_size(t)) + type_size(t);
      }
    }
  }
  return ALIGN(work_size, 8);
}

const Type *get_small_struct_elem_type(const Type *type) {
  for (;;) {
    if (type->kind != TY_STRUCT)
      return type;
    const StructInfo *sinfo = type->struct_.info;
    assert(sinfo != NULL);
    assert(sinfo->member_count > 0);
    type = sinfo->members[0].type;
  }
}
