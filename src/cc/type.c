#include "type.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "table.h"
#include "util.h"
#include "var.h"  // VarInfo

const Type tyChar =          {.kind=TY_FIXNUM, .fixnum={.kind=FX_CHAR,  .is_unsigned=false}};
const Type tyShort =         {.kind=TY_FIXNUM, .fixnum={.kind=FX_SHORT, .is_unsigned=false}};
const Type tyInt =           {.kind=TY_FIXNUM, .fixnum={.kind=FX_INT,   .is_unsigned=false}};
const Type tyLong =          {.kind=TY_FIXNUM, .fixnum={.kind=FX_LONG,  .is_unsigned=false}};
const Type tyLLong =         {.kind=TY_FIXNUM, .fixnum={.kind=FX_LLONG, .is_unsigned=false}};
const Type tyUnsignedChar =  {.kind=TY_FIXNUM, .fixnum={.kind=FX_CHAR,  .is_unsigned=true}};
const Type tyUnsignedShort = {.kind=TY_FIXNUM, .fixnum={.kind=FX_SHORT, .is_unsigned=true}};
const Type tyUnsignedInt =   {.kind=TY_FIXNUM, .fixnum={.kind=FX_INT,   .is_unsigned=true}};
const Type tyUnsignedLong =  {.kind=TY_FIXNUM, .fixnum={.kind=FX_LONG,  .is_unsigned=true}};
const Type tyUnsignedLLong = {.kind=TY_FIXNUM, .fixnum={.kind=FX_LLONG, .is_unsigned=true}};
const Type tyEnum =          {.kind=TY_FIXNUM, .fixnum={.kind=FX_ENUM}};
const Type tyVoid =          {.kind=TY_VOID};
const Type tyVoidPtr =       {.kind=TY_PTR, .pa={.ptrof=&tyVoid}};

size_t num_size_table[]  = {1, 2, 4, 8, 8, 4};
int    num_align_table[] = {1, 2, 4, 8, 8, 4};

void set_fixnum_size(enum FixnumKind kind, size_t size, int align) {
  num_size_table[kind] = size;
  num_align_table[kind] = align;
}

static void calc_struct_size(StructInfo *sinfo) {
  assert(sinfo != NULL);
  if (sinfo->size >= 0)
    return;

  size_t size = 0;
  size_t maxsize = 0;
  int max_align = 1;

  for (int i = 0, len = sinfo->members->len; i < len; ++i) {
    VarInfo *member = sinfo->members->data[i];
    size_t sz = type_size(member->type);
    int align = align_size(member->type);
    size = ALIGN(size, align);
    member->struct_.offset = size;
    if (!sinfo->is_union) {
      size += sz;
    } else {
      if (maxsize < sz)
        maxsize = sz;
    }
    if (max_align < align)
      max_align = align;
  }

  if (sinfo->is_union)
    size = maxsize;
  size = ALIGN(size, max_align);
  sinfo->size = size;
  sinfo->align = max_align;
}

size_t type_size(const Type *type) {
  switch (type->kind) {
  case TY_VOID:
    return 1;  // ?
  case TY_FIXNUM:
    return num_size_table[type->fixnum.kind];
  case TY_PTR:
    return 8;
  case TY_ARRAY:
    assert(type->pa.length != (size_t)-1);
    return type_size(type->pa.ptrof) * type->pa.length;
  case TY_FUNC:
    return 1;
  case TY_STRUCT:
    calc_struct_size(type->struct_.info);
    return type->struct_.info->size;
  default:
    assert(false);
    return 1;
  }
}

int align_size(const Type *type) {
  switch (type->kind) {
  case TY_VOID:
    return 1;  // ?
  case TY_FIXNUM:
    return num_align_table[type->fixnum.kind];
  case TY_PTR:
  case TY_FUNC:
    return 8;
  case TY_ARRAY:
    return align_size(type->pa.ptrof);
  case TY_STRUCT:
    calc_struct_size(type->struct_.info);
    return type->struct_.info->align;
  default:
    assert(false);
    return 1;
  }
}

bool is_fixnum(enum TypeKind kind) {
  return kind == TY_FIXNUM;
}

bool is_char_type(const Type *type) {
  return type->kind == TY_FIXNUM && type->fixnum.kind == FX_CHAR;
}

bool is_void_ptr(const Type *type) {
  return type->kind == TY_PTR && type->pa.ptrof->kind == TY_VOID;
}

bool ptr_or_array(const Type *type) {
  return type->kind == TY_PTR || type->kind == TY_ARRAY;
}

Type *ptrof(const Type *type) {
  Type *ptr = malloc(sizeof(*ptr));
  ptr->kind = TY_PTR;
  ptr->pa.ptrof = type;
  return ptr;
}

const Type *array_to_ptr(const Type *type) {
  assert(type->kind == TY_ARRAY);
  return ptrof(type->pa.ptrof);
}

Type *arrayof(const Type *type, size_t length) {
  Type *arr = malloc(sizeof(*arr));
  arr->kind = TY_ARRAY;
  arr->pa.ptrof = type;
  arr->pa.length = length;
  return arr;
}

Type *new_func_type(const Type *ret, Vector *params, Vector *param_types, bool vaargs) {
  Type *f = malloc(sizeof(*f));
  f->kind = TY_FUNC;
  f->func.ret = ret;
  f->func.vaargs = vaargs;
  f->func.params = params;
  f->func.param_types = param_types;
  return f;
}

// Struct

StructInfo *create_struct(Vector *members, bool is_union) {
  StructInfo *sinfo = malloc(sizeof(*sinfo));
  sinfo->members = members;
  sinfo->is_union = is_union;
  sinfo->size = -1;
  sinfo->align = 0;
  calc_struct_size(sinfo);
  return sinfo;
}

// Enum

Table enum_table;

Type *find_enum(const Name *name) {
  return table_get(&enum_table, name);
}

Type *define_enum(const Name *ident) {
  Type *type = malloc(sizeof(*type));
  type->kind = TY_FIXNUM;
  type->fixnum.kind = FX_ENUM;
  type->fixnum.is_unsigned = false;
  type->fixnum.enum_.ident = ident;

  if (ident != NULL)
    table_put(&enum_table, ident, type);

  return type;
}

#if 0
void dump_type(FILE *fp, const Type *type) {
  switch (type->kind) {
  case TY_VOID: fprintf(fp, "void"); break;
  case TY_FIXNUM:
    switch (type->fixnum.kind) {
    case FX_CHAR:  fprintf(fp, "char"); break;
    case FX_SHORT: fprintf(fp, "short"); break;
    case FX_INT:   fprintf(fp, "int"); break;
    case FX_LONG:  fprintf(fp, "long"); break;
    case FX_ENUM:  fprintf(fp, "enum"); break;
    default: assert(false); break;
    }
    break;
  case TY_PTR: dump_type(fp, type->pa.ptrof); fprintf(fp, "*"); break;
  case TY_ARRAY: dump_type(fp, type->pa.ptrof); fprintf(fp, "[%d]", (int)type->pa.length); break;
  default: assert(false); break;
  }
}
#endif

// Typedef

Table typedef_table;  // <const Name*, Type*>

const Type *find_typedef(const Name *name) {
  return table_get(&typedef_table, name);
}

bool add_typedef(const Name *name, const Type *type) {
  if (table_get(&typedef_table, name) != NULL)
    return false;
  table_put(&typedef_table, name, (void*)type);
  return true;
}
