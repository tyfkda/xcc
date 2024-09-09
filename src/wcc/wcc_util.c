#include "wcc.h"

#include <assert.h>
#include <stdlib.h>  // realloc, free
#include <string.h>

#include "ast.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "wasm.h"

const char SP_NAME[] = "__stack_pointer";  // Variable name for stack pointer (global).
const char BREAK_ADDRESS_NAME[] = "__curbrk";

bool verbose;
Table func_info_table;
Table gvar_info_table;
Vector *functypes;  // <DataStorage*>
Table indirect_function_table;
Vector *tags;
Vector *init_funcs;

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
    TagInfo *t = tags->data[i];
    if (equal_name(t->name, name)) {
      if (t->typeindex != typeindex)
        error("Tag type mismatch: %.*s", NAMES(name));
      return t;
    }
  }

  TagInfo *t = calloc_or_die(sizeof(*t));
  t->name = name;
  t->typeindex = typeindex;
  t->index = len;
  vec_push(tags, t);
  return t;
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
  if (equal_name(varinfo->name, alloc_name(SP_NAME, NULL, false)) ||
      equal_name(varinfo->name, alloc_name(BREAK_ADDRESS_NAME, NULL, false)))
    return false;
#endif
  return true;
}
