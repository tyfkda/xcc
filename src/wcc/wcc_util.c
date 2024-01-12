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

bool verbose;
enum OutType out_type;
Table func_info_table;
Table gvar_info_table;
Vector *functypes;  // <DataStorage*>
Table indirect_function_table;
Vector *tags;

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

int getsert_tag(int typeindex) {
  int len = tags->len;
  for (int i = 0; i < len; ++i) {
    int t = VOIDP2INT(tags->data[i]);
    if (t == typeindex)
      return i;
  }
  vec_push(tags, INT2VOIDP(typeindex));
  return len;
}

//

void write_wasm_header(FILE *ofp) {
  WasmHeader header = {
    WASM_BINARY_MAGIC,
    WASM_BINARY_VERSION,
  };
  fwrite(&header, sizeof(header), 1, ofp);
}

void data_release(DataStorage *data) {
  if (data->chunk_stack != NULL) {
    free_vector(data->chunk_stack);
    data->chunk_stack = NULL;
  }
  if (data->buf != NULL) {
    free(data->buf);
    data_init(data);
  }
}

void data_init(DataStorage *data) {
  data->chunk_stack = NULL;
  data->buf = NULL;
  data->capacity = 0;
  data->len = 0;
}

void data_reserve(DataStorage *data, size_t capacity) {
  if (data->capacity < capacity) {
    const size_t MIN = 16;
    size_t c = data->capacity << 1;
    if (c > capacity)
      capacity = c;
    if (MIN > capacity)
      capacity = MIN;
    data->buf = realloc_or_die(data->buf, sizeof(*data->buf) * capacity);
    data->capacity = capacity;
  }
}

void data_insert(DataStorage *data, ssize_t _pos, const unsigned char *buf, size_t size) {
  size_t pos = _pos == -1 ? data->len : (size_t)_pos;
  assert(/* 0 <= pos && */ pos <= data->len);
  size_t newlen = data->len + size;
  data_reserve(data, newlen);
  if (pos < data->len)
    memmove(data->buf + pos + size, data->buf + pos, data->len - pos);
  memcpy(data->buf + pos, buf, size);
  data->len = newlen;
}

void data_append(DataStorage *data, const unsigned char *buf, size_t size) {
  data_insert(data, -1, buf, size);
}

void data_push(DataStorage *data, unsigned char c) {
  unsigned char buf[1] = {c};
  data_insert(data, -1, buf, 1);
}

void data_concat(DataStorage *dst, DataStorage *src) {
  data_insert(dst, -1, src->buf, src->len);
}

void data_leb128(DataStorage *data, ssize_t pos, int64_t val) {
  unsigned char buf[12], *p = buf;
  const int64_t MAX = 1 << 6;
  for (;;) {
    if (val < MAX && val >= -MAX) {
      *p++ = val & 0x7f;
      data_insert(data, pos, buf, p - buf);
      return;
    }
    *p++ = (val & 0x7f) | 0x80;
    val >>= 7;
  }
}

void data_uleb128(DataStorage *data, ssize_t pos, uint64_t val) {
  unsigned char buf[12], *p = buf;
  const uint64_t MAX = 1 << 7;
  for (;;) {
    if (val < MAX) {
      *p++ = val & 0x7f;
      data_insert(data, pos, buf, p - buf);
      return;
    }
    *p++ = (val & 0x7f) | 0x80;
    val >>= 7;
  }
}

void data_string(DataStorage *data, const void *str, size_t len) {
  data_uleb128(data, -1, len);
  data_append(data, (const unsigned char*)str, len);
}

void data_varuint32(DataStorage *data, ssize_t pos, uint64_t val) {
  unsigned char buf[5], *p = buf;
  for (int i = 0; i < 4; ++i) {
    *p++ = (val & 0x7f) | 0x80;
    val >>= 7;
  }
  *p++ = val & 0x7f;
  data_insert(data, pos, buf, p - buf);
}

void data_open_chunk(DataStorage *data) {
  Vector *stack = data->chunk_stack;
  if (stack == NULL)
    data->chunk_stack = stack = new_vector();
  vec_push(stack, INT2VOIDP(data->len));
}

void data_close_chunk(DataStorage *data, ssize_t num) {
  Vector *stack = data->chunk_stack;
  assert(stack != NULL && stack->len > 0);
  size_t pos = VOIDP2INT(vec_pop(stack));
  if (num == (ssize_t)-1)
    num = data->len - pos;
  data_uleb128(data, pos, num);
}

//

Expr *get_sp_var(void) {
  static Expr *spvar;
  if (spvar == NULL) {
    const Name *spname = alloc_name(SP_NAME, NULL, false);
    GVarInfo *info = get_gvar_info_from_name(spname);
    assert(info != NULL);
    spvar = new_expr_variable(spname, info->varinfo->type, NULL, global_scope);
  }
  return spvar;
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
  // Special: Stack pointer.
  if (equal_name(varinfo->name, alloc_name(SP_NAME, NULL, false)))
    return false;
#endif
  return true;
}
