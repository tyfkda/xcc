#include "type.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "table.h"
#include "util.h"
#include "var.h"  // VarInfo

const Type tyChar =          {.kind=TY_FIXNUM, .fixnum={.kind=FX_CHAR,  .is_unsigned=false}};
const Type tyInt =           {.kind=TY_FIXNUM, .fixnum={.kind=FX_INT,   .is_unsigned=false}};
const Type tyUnsignedChar =  {.kind=TY_FIXNUM, .fixnum={.kind=FX_CHAR,  .is_unsigned=true}};
const Type tyUnsignedInt =   {.kind=TY_FIXNUM, .fixnum={.kind=FX_INT,   .is_unsigned=true}};
const Type tyEnum =          {.kind=TY_FIXNUM, .fixnum={.kind=FX_ENUM}};
const Type tyVoid =          {.kind=TY_VOID};
const Type tyVoidPtr =       {.kind=TY_PTR, .pa={.ptrof=&tyVoid}};
const Type tyBool =          {.kind=TY_FIXNUM, .fixnum={.kind=FX_INT,   .is_unsigned=false}};
const Type tySize =          {.kind=TY_FIXNUM, .fixnum={.kind=FX_LONG,  .is_unsigned=true}};
const Type tySSize =         {.kind=TY_FIXNUM, .fixnum={.kind=FX_LONG,  .is_unsigned=false}};
#ifndef __NO_FLONUM
const Type tyFloat =         {.kind=TY_FLONUM};  // TODO:
const Type tyDouble =        {.kind=TY_FLONUM};
#endif

#define FIXNUM_TABLE(uns, qual) \
    { \
      {.kind=TY_FIXNUM, .fixnum={.kind=FX_CHAR,  .is_unsigned=uns}, .qualifier=qual}, \
      {.kind=TY_FIXNUM, .fixnum={.kind=FX_SHORT, .is_unsigned=uns}, .qualifier=qual}, \
      {.kind=TY_FIXNUM, .fixnum={.kind=FX_INT,   .is_unsigned=uns}, .qualifier=qual}, \
      {.kind=TY_FIXNUM, .fixnum={.kind=FX_LONG,  .is_unsigned=uns}, .qualifier=qual}, \
      {.kind=TY_FIXNUM, .fixnum={.kind=FX_LLONG, .is_unsigned=uns}, .qualifier=qual}, \
    }

static const Type kFixnumTypeTable[2][4][FX_LLONG + 1] = {
  {
    FIXNUM_TABLE(false, 0), FIXNUM_TABLE(false, 1), FIXNUM_TABLE(false, 2), FIXNUM_TABLE(false, 3),
  },
  {
    FIXNUM_TABLE(true, 0), FIXNUM_TABLE(true, 1), FIXNUM_TABLE(true, 2), FIXNUM_TABLE(true, 3),
  },
};
#undef FIXNUM_TABLE

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
    member->struct_member.offset = size;
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
#ifndef __NO_FLONUM
  case TY_FLONUM:
    return 8;
#endif
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
#ifndef __NO_FLONUM
  case TY_FLONUM:
    return 8;
#endif
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

bool is_number(const Type *type) {
#ifndef __NO_FLONUM
  if (is_flonum(type))
    return true;
#endif
  return is_fixnum(type->kind);
}

#ifndef __NO_FLONUM
bool is_flonum(const Type *type) {
  return type->kind == TY_FLONUM;
}
#endif

bool is_char_type(const Type *type) {
  return type->kind == TY_FIXNUM && type->fixnum.kind == FX_CHAR;
}

bool is_void_ptr(const Type *type) {
  return type->kind == TY_PTR && type->pa.ptrof->kind == TY_VOID;
}

bool ptr_or_array(const Type *type) {
  return type->kind == TY_PTR || type->kind == TY_ARRAY;
}

const Type *get_fixnum_type(enum FixnumKind kind, bool is_unsigned, int qualifier) {
  assert(kind != FX_ENUM);
  return &kFixnumTypeTable[is_unsigned][qualifier & 3][kind];
}

Type *ptrof(const Type *type) {
  Type *ptr = malloc(sizeof(*ptr));
  ptr->kind = TY_PTR;
  ptr->qualifier = 0;
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
  arr->qualifier = 0;
  arr->pa.ptrof = type;
  arr->pa.length = length;
  return arr;
}

Type *new_func_type(const Type *ret, Vector *params, Vector *param_types, bool vaargs) {
  Type *f = malloc(sizeof(*f));
  f->kind = TY_FUNC;
  f->qualifier = 0;
  f->func.ret = ret;
  f->func.vaargs = vaargs;
  f->func.params = params;
  f->func.param_types = param_types;
  return f;
}

const Type *const_type(const Type *type) {
  if (type->qualifier & TQ_CONST)
    return type;
  Type *ctype = malloc(sizeof(*ctype));
  memcpy(ctype, type, sizeof(*ctype));
  ctype->qualifier |= TQ_CONST;
  return ctype;
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

Type *create_enum_type(const Name *name) {
  Type *type = malloc(sizeof(*type));
  type->kind = TY_FIXNUM;
  type->qualifier = 0;
  type->fixnum.kind = FX_ENUM;
  type->fixnum.is_unsigned = false;
  type->fixnum.enum_.ident = name;
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

bool same_type(const Type *type1, const Type *type2) {
  for (;;) {
    if (type1->kind != type2->kind)
      return false;

    switch (type1->kind) {
    case TY_VOID:
      return true;
    case TY_FIXNUM:
      return type1->fixnum.kind == type2->fixnum.kind &&
          type1->fixnum.is_unsigned == type2->fixnum.is_unsigned;
#ifndef __NO_FLONUM
    case TY_FLONUM:
      return true;
#endif
    case TY_ARRAY:
      if (type1->pa.length != type2->pa.length)
        return false;
      // Fallthrough
    case TY_PTR:
      type1 = type1->pa.ptrof;
      type2 = type2->pa.ptrof;
      continue;
    case TY_FUNC:
      if (!same_type(type1->func.ret, type2->func.ret) || type1->func.vaargs != type2->func.vaargs)
        return false;
      if (type1->func.param_types == NULL && type2->func.param_types == NULL)
        return true;
      if (type1->func.param_types == NULL || type2->func.param_types == NULL ||
          type1->func.param_types->len != type2->func.param_types->len)
        return false;
      for (int i = 0, len = type1->func.param_types->len; i < len; ++i) {
        const Type *t1 = (const Type*)type1->func.param_types->data[i];
        const Type *t2 = (const Type*)type2->func.param_types->data[i];
        if (!same_type(t1, t2))
          return false;
      }
      return true;
    case TY_STRUCT:
      {
        if (type1->struct_.info != NULL) {
          if (type2->struct_.info != NULL)
            return type1->struct_.info == type2->struct_.info;
        }
        if (type1->struct_.name == NULL || type2->struct_.name == NULL)
          return false;
        return equal_name(type1->struct_.name, type2->struct_.name);
      }
    }
  }
}

bool can_cast(const Type *dst, const Type *src, bool zero, bool is_explicit) {
  if (same_type(dst, src))
    return true;

  if (dst->kind == TY_VOID)
    return src->kind == TY_VOID || is_explicit;
  if (src->kind == TY_VOID)
    return false;

  switch (dst->kind) {
  case TY_FIXNUM:
    switch (src->kind) {
    case TY_FIXNUM:
#ifndef __NO_FLONUM
    case TY_FLONUM:
#endif
      return true;
    case TY_PTR:
    case TY_ARRAY:
    case TY_FUNC:
      if (is_explicit) {
        // TODO: Check sizeof(long) is same as sizeof(ptr)
        return true;
      }
      break;
    default:
      break;
    }
    break;
#ifndef __NO_FLONUM
  case TY_FLONUM:
    switch (src->kind) {
    case TY_FIXNUM:
      return true;
    default:
      break;
    }
    break;
#endif
  case TY_PTR:
    switch (src->kind) {
    case TY_FIXNUM:
      if (zero)  // Special handling for 0 to pointer.
        return true;
      if (is_explicit)
        return true;
      break;
    case TY_PTR:
      if (is_explicit)
        return true;
      // void* is interchangable with any pointer type.
      if (dst->pa.ptrof->kind == TY_VOID || src->pa.ptrof->kind == TY_VOID)
        return true;
      if (src->pa.ptrof->kind == TY_FUNC)
        return can_cast(dst, src->pa.ptrof, zero, is_explicit);
      break;
    case TY_ARRAY:
      if (is_explicit)
        return true;
      if (same_type(dst->pa.ptrof, src->pa.ptrof) ||
          can_cast(dst, ptrof(src->pa.ptrof), zero, is_explicit))
        return true;
      break;
    case TY_FUNC:
      if (is_explicit)
        return true;
      switch (dst->pa.ptrof->kind) {
      case TY_FUNC:
        {
          const Type *ftype = dst->pa.ptrof;
          return (same_type(ftype, src) ||
                  (ftype->func.param_types == NULL || src->func.param_types == NULL));
        }
      case TY_VOID:
        return true;
      default:
        break;
      }
      break;
    default:  break;
    }
    break;
  case TY_ARRAY:
    switch (src->kind) {
    case TY_PTR:
      if (is_explicit && same_type(dst->pa.ptrof, src->pa.ptrof))
        return true;
      // Fallthrough
    case TY_ARRAY:
      if (is_explicit)
        return true;
      break;
    default:  break;
    }
    break;
  default:
    break;
  }
  return false;
}
