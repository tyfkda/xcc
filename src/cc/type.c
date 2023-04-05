#include "../config.h"
#include "type.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "table.h"
#include "util.h"

Type tyChar =          {.kind=TY_FIXNUM, .fixnum={.kind=FX_CHAR,  .is_unsigned=false}};
Type tyInt =           {.kind=TY_FIXNUM, .fixnum={.kind=FX_INT,   .is_unsigned=false}};
Type tyUnsignedChar =  {.kind=TY_FIXNUM, .fixnum={.kind=FX_CHAR,  .is_unsigned=true}};
Type tyUnsignedInt =   {.kind=TY_FIXNUM, .fixnum={.kind=FX_INT,   .is_unsigned=true}};
Type tyEnum =          {.kind=TY_FIXNUM, .fixnum={.kind=FX_ENUM}};
Type tyVoid =          {.kind=TY_VOID};
Type tyConstVoid =     {.kind=TY_VOID, .qualifier=TQ_CONST};
Type tyVoidPtr =       {.kind=TY_PTR, .pa={.ptrof=&tyVoid}};
Type tyBool =          {.kind=TY_FIXNUM, .fixnum={.kind=FX_INT,   .is_unsigned=false}};
Type tySize =          {.kind=TY_FIXNUM, .fixnum={.kind=FX_LONG,  .is_unsigned=true}};
Type tySSize =         {.kind=TY_FIXNUM, .fixnum={.kind=FX_LONG,  .is_unsigned=false}};
#ifndef __NO_FLONUM
Type tyFloat =         {.kind=TY_FLONUM, .flonum={.kind=FL_FLOAT}};
Type tyDouble =        {.kind=TY_FLONUM, .flonum={.kind=FL_DOUBLE}};
#endif

#define FIXNUM_TABLE(uns, qual) \
    { \
      {.kind=TY_FIXNUM, .fixnum={.kind=FX_CHAR,  .is_unsigned=uns}, .qualifier=qual}, \
      {.kind=TY_FIXNUM, .fixnum={.kind=FX_SHORT, .is_unsigned=uns}, .qualifier=qual}, \
      {.kind=TY_FIXNUM, .fixnum={.kind=FX_INT,   .is_unsigned=uns}, .qualifier=qual}, \
      {.kind=TY_FIXNUM, .fixnum={.kind=FX_LONG,  .is_unsigned=uns}, .qualifier=qual}, \
      {.kind=TY_FIXNUM, .fixnum={.kind=FX_LLONG, .is_unsigned=uns}, .qualifier=qual}, \
    }

static Type kFixnumTypeTable[2][4][FX_LLONG + 1] = {
  {
    FIXNUM_TABLE(false, 0), FIXNUM_TABLE(false, 1), FIXNUM_TABLE(false, 2), FIXNUM_TABLE(false, 3),
  },
  {
    FIXNUM_TABLE(true, 0), FIXNUM_TABLE(true, 1), FIXNUM_TABLE(true, 2), FIXNUM_TABLE(true, 3),
  },
};
#undef FIXNUM_TABLE

size_t fixnum_size_table[]  = {1, 2, 4, 8, 8, 4};
int    fixnum_align_table[] = {1, 2, 4, 8, 8, 4};

#ifndef __NO_FLONUM
size_t flonum_size_table[]  = {4, 8};
int    flonum_align_table[] = {4, 8};
#endif

void set_fixnum_size(enum FixnumKind kind, size_t size, int align) {
  fixnum_size_table[kind] = size;
  fixnum_align_table[kind] = align;
}

static size_t calc_bitfield_size(StructInfo *sinfo, int *pi, size_t size, size_t *palign) {
  // Detect initial fixnum kind.
  int i = *pi;
  enum FixnumKind kind;
  {
    MemberInfo *minfo = &sinfo->members[i];
    size_t align = align_size(minfo->type);
    size_t remain = size % align;
    if (remain == 0) {
      kind = minfo->type->fixnum.kind;
    } else {
      // Detect LSB.
      ssize_t s;
      for (s = 1; (remain & s) == 0; s <<= 1)
        ;
      ssize_t d =  s - (ssize_t)type_size(&tyInt);
      if (d < 0) {
        for (kind = FX_INT; kind > FX_CHAR; --kind) {
          if (s >= (ssize_t)type_size(get_fixnum_type(kind, false, 0)))
            break;
        }
      } else {
        for (kind = FX_INT; kind < FX_LLONG; ++kind) {
          if (s <= (ssize_t)type_size(get_fixnum_type(kind, false, 0)))
            break;
        }
      }
    }
  }

  const Type *bitfield_type = get_fixnum_type(kind, false, 0);
  unsigned int s = type_size(bitfield_type) * TARGET_CHAR_BIT;
  unsigned int bit_position = 0;
  for (int len = sinfo->member_count; i < len && bit_position != s; ++i) {
    MemberInfo *minfo = &sinfo->members[i];
    if (minfo->bitfield.width <= 0)
      break;

    if (bit_position + minfo->bitfield.width > s) {
      enum FixnumKind k;
      for (k = kind; ++k <= FX_LLONG; ) {
        const Type *t = get_fixnum_type(k, false, 0);
        unsigned int ss = type_size(t) * TARGET_CHAR_BIT;
        if (bit_position + minfo->bitfield.width <= ss) {
          bitfield_type = t;
          s = ss;
          kind = k;
          break;
        }
      }
      if (k > FX_LLONG)
        break;
    }

    minfo->offset = size;
    minfo->bitfield.position = bit_position;
    bit_position += minfo->bitfield.width;
  }

  size_t align = align_size(bitfield_type);
  size = ALIGN(size + type_size(bitfield_type), align);

  // Write back base kind.
  for (int j = *pi; j < i; ++j) {
    MemberInfo *minfo = &sinfo->members[j];
    minfo->bitfield.base_kind = kind;
  }

  *pi = i;
  *palign = align;
  return size;
}

static void calc_struct_size(StructInfo *sinfo) {
  assert(sinfo != NULL);
  if (sinfo->size >= 0)
    return;

  size_t size = 0;
  size_t max_align = 1;

  for (int i = 0, len = sinfo->member_count; i < len; ++i) {
    MemberInfo *minfo = &sinfo->members[i];
    size_t sz = type_size(minfo->type);
    size_t align = align_size(minfo->type);
    if (!sinfo->is_union) {
      if (minfo->bitfield.width > 0) {
        size = calc_bitfield_size(sinfo, &i, size, &align);
        --i;
      } else {
        size = ALIGN(size, align);
        minfo->offset = size;
        size += sz;
      }
    } else {
      minfo->offset = 0;
      if (size < sz)
        size = sz;
    }
    if (max_align < align)
      max_align = align;
  }

  size = ALIGN(size, max_align);
  sinfo->size = size;
  sinfo->align = max_align;
}

size_t type_size(const Type *type) {
  switch (type->kind) {
  case TY_VOID:
    return 1;
  case TY_FIXNUM:
    return fixnum_size_table[type->fixnum.kind];
#ifndef __NO_FLONUM
  case TY_FLONUM:
    return flonum_size_table[type->flonum.kind];
#endif
  case TY_PTR:
    return fixnum_size_table[FX_LONG];
  case TY_ARRAY:
    assert(type->pa.length >= 0);
    return type_size(type->pa.ptrof) * type->pa.length;
  case TY_FUNC:
    return 1;
  case TY_STRUCT:
    assert(type->struct_.info->size >= 0);
    return type->struct_.info->size;
  default:
    assert(false);
    return 1;
  }
}

size_t align_size(const Type *type) {
  switch (type->kind) {
  case TY_VOID:
    return 1;  // Just in case.
  case TY_FIXNUM:
    return fixnum_align_table[type->fixnum.kind];
#ifndef __NO_FLONUM
  case TY_FLONUM:
    return flonum_align_table[type->fixnum.kind];
#endif
  case TY_PTR:
    return fixnum_align_table[FX_LONG];
  case TY_FUNC:
    return 1;
  case TY_ARRAY:
    return align_size(type->pa.ptrof);
  case TY_STRUCT:
    assert(type->struct_.info->align > 0);
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

bool is_prim_type(const Type *type) {
  return is_number(type) || type->kind == TY_PTR;
}

bool ptr_or_array(const Type *type) {
  return type->kind == TY_PTR || type->kind == TY_ARRAY;
}

Type *get_fixnum_type(enum FixnumKind kind, bool is_unsigned, int qualifier) {
  assert(kind != FX_ENUM);
  return &kFixnumTypeTable[is_unsigned][qualifier & 3][kind];
}

Type *ptrof(Type *type) {
  Type *ptr = malloc(sizeof(*ptr));
  ptr->kind = TY_PTR;
  ptr->qualifier = 0;
  ptr->pa.ptrof = type;
  return ptr;
}

Type *array_to_ptr(Type *type) {
  assert(type->kind == TY_ARRAY);
  return ptrof(type->pa.ptrof);
}

Type *arrayof(Type *type, ssize_t length) {
  Type *arr = malloc(sizeof(*arr));
  arr->kind = TY_ARRAY;
  arr->qualifier = 0;
  arr->pa.ptrof = type;
  arr->pa.length = length;
  return arr;
}

Type *new_func_type(Type *ret, const Vector *params, const Vector *param_types, bool vaargs) {
  Type *f = malloc(sizeof(*f));
  f->kind = TY_FUNC;
  f->qualifier = 0;
  f->func.ret = ret;
  f->func.vaargs = vaargs;
  f->func.params = params;
  f->func.param_types = param_types;
  return f;
}

Type *qualified_type(Type *type, int additional) {
  int modified = type->qualifier | additional;
  if (modified == type->qualifier)
    return type;
  Type *ctype = clone_type(type);
  ctype->qualifier = modified;
  return ctype;
}

Type *clone_type(const Type *type) {
  Type *cloned = malloc(sizeof(*cloned));
  *cloned = *type;
  return cloned;
}

Type *get_callee_type(Type *type) {
  if (type->kind == TY_PTR)
    type = type->pa.ptrof;
  return type->kind == TY_FUNC ? type : NULL;
}

// Struct
StructInfo *create_struct_info(MemberInfo *members, int count, bool is_union) {
  StructInfo *sinfo = malloc(sizeof(*sinfo));
  sinfo->members = members;
  sinfo->member_count = count;
  sinfo->is_union = is_union;
  sinfo->size = -1;
  sinfo->align = 0;
  calc_struct_size(sinfo);
  return sinfo;
}

Type *create_struct_type(StructInfo *sinfo, const Name *name, int qualifier) {
  Type *type = malloc(sizeof(*type));
  type->kind = TY_STRUCT;
  type->qualifier = qualifier;
  type->struct_.name = name;
  type->struct_.info = sinfo;
  return type;
}

int find_struct_member(const StructInfo *sinfo, const Name *name) {
  const MemberInfo *members = sinfo->members;
  for (int i = 0, len = sinfo->member_count; i < len; ++i) {
    const MemberInfo *info = &members[i];
    if (info->name != NULL && equal_name(info->name, name))
      return i;
  }
  return -1;
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

bool same_type_without_qualifier(const Type *type1, const Type *type2, bool ignore_qualifier) {
  for (;;) {
    if (type1->kind != type2->kind || (!ignore_qualifier && type1->qualifier != type2->qualifier))
      return false;

    switch (type1->kind) {
    case TY_VOID:
      return true;
    case TY_FIXNUM:
      return type1->fixnum.kind == type2->fixnum.kind &&
          type1->fixnum.is_unsigned == type2->fixnum.is_unsigned;
#ifndef __NO_FLONUM
    case TY_FLONUM:
      return type1->flonum.kind == type2->flonum.kind;
#endif
    case TY_ARRAY:
      if (type1->pa.length != type2->pa.length &&
          type1->pa.length != -1 && type2->pa.length != -1)
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
        const Type *t1 = type1->func.param_types->data[i];
        const Type *t2 = type2->func.param_types->data[i];
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

bool same_type(const Type *type1, const Type *type2) {
  return same_type_without_qualifier(type1, type2, false);
}

bool can_cast(const Type *dst, const Type *src, bool zero, bool is_explicit) {
  if (same_type_without_qualifier(dst, src, dst->kind == TY_STRUCT))
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
    case TY_FLONUM:
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
    case TY_ARRAY:
      if (is_explicit)
        return true;
      // Fallthrough
    case TY_PTR:
      if (is_explicit)
        return true;
      // non-const <- const implicitly.
      if ((src->pa.ptrof->qualifier & TQ_CONST) && !(dst->pa.ptrof->qualifier & TQ_CONST)) {
        // Allow const char to char for string literal, otherwise disallow.
        // TODO: Allows string literal only.
        return is_char_type(src->pa.ptrof) && (is_char_type(dst->pa.ptrof) || dst->pa.ptrof->kind == TY_VOID);
      }
      // void* is interchangable with any pointer type.
      if (dst->pa.ptrof->kind == TY_VOID || src->pa.ptrof->kind == TY_VOID)
        return true;
      if (src->pa.ptrof->kind == TY_FUNC)
        return can_cast(dst, src->pa.ptrof, zero, is_explicit);
      return same_type_without_qualifier(dst->pa.ptrof, src->pa.ptrof, true);
    case TY_FUNC:
      if (is_explicit)
        return true;
      switch (dst->pa.ptrof->kind) {
      case TY_FUNC:
        {
          const Type *ftype = dst->pa.ptrof;
          return (same_type(ftype, src) ||
                  (same_type(ftype->func.ret, src->func.ret) && (ftype->func.param_types == NULL || src->func.param_types == NULL)));
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

//

typedef struct PrintTypeChain PrintTypeChain;
struct PrintTypeChain {
  struct PrintTypeChain *parent;
  void (*callback)(FILE *fp, const Type *type);
  const Type *type;
};

static void call_print_type_chain(const PrintTypeChain *chain, FILE *fp) {
  for (; chain != NULL; chain = chain->parent)
    (*chain->callback)(fp, chain->type);
}

static void print_func_params(FILE *fp, const Type *type) {
  assert(type->kind == TY_FUNC);
  fprintf(fp, "(");
  if (type->func.param_types != NULL) {
    int param_count = type->func.param_types->len;
    if (param_count == 0 && !type->func.vaargs) {
      fprintf(fp, "void");
    } else {
      for (int i = 0; i < param_count; ++i) {
        if (i > 0)
          fprintf(fp, ", ");
        print_type(fp, type->func.param_types->data[i]);
      }
      if (type->func.vaargs) {
        if (param_count > 0)
          fprintf(fp, ", ");
        fprintf(fp, "...");
      }
    }
  }
  fprintf(fp, ")");
}

static void print_ptr_type(FILE *fp, const Type *type) {
  fprintf(fp, "*");
  if (type->qualifier & TQ_CONST)
    fprintf(fp, " const");
  if (type->qualifier & TQ_VOLATILE)
    fprintf(fp, " volatile");
}

static void print_nested_ptr_type(FILE *fp, const Type *type) {
  fprintf(fp, "(");
  for (const Type *p = type; p->kind == TY_PTR; p = p->pa.ptrof)
    fprintf(fp, "*");
}

static void print_nested_ptr_type2(FILE *fp, const Type *_type) {
  UNUSED(_type);
  fprintf(fp, ")");
}

static void print_array_type(FILE *fp, const Type *type) {
  for (; type->kind == TY_ARRAY; type = type->pa.ptrof) {
    if (type->pa.length > 0)
      fprintf(fp, "[%zu]", type->pa.length);
    else
      fprintf(fp, "[]");
  }
}

void print_type_recur(FILE *fp, const Type *type, PrintTypeChain *parent) {
  if (type->kind != TY_PTR) {
    if (type->qualifier & TQ_CONST)
      fprintf(fp, "const ");
    if (type->qualifier & TQ_VOLATILE)
      fprintf(fp, "volatile ");
  }

  switch (type->kind) {
  case TY_VOID:
    fprintf(fp, "void");
    call_print_type_chain(parent, fp);
    break;
  case TY_FIXNUM:
    {
      enum FixnumKind kind = type->fixnum.kind;
      if (kind == FX_ENUM) {
        if (type->fixnum.enum_.ident != NULL)
          fprintf(fp, "enum %.*s", type->fixnum.enum_.ident->bytes, type->fixnum.enum_.ident->chars);
        else
          fprintf(fp, "enum (anonymous)");
      } else {
        static const char *names[] = {"char", "short", "int", "long", "long long"};
        assert(kind >= 0 && kind < sizeof(names) / sizeof(*names));
        const char *sign = type->fixnum.is_unsigned ? "unsigned " : "";
        fprintf(fp, "%s%s", sign, names[kind]);
      }
      call_print_type_chain(parent, fp);
    }
    break;
#ifndef __NO_FLONUM
  case TY_FLONUM:
    switch (type->flonum.kind) {
    case FL_FLOAT:  fprintf(fp, "float"); break;
    case FL_DOUBLE: fprintf(fp, "double"); break;
    default: assert(false); break;
    }
    call_print_type_chain(parent, fp);
    break;
#endif
  case TY_PTR:
    {
      const Type *nestedtype = NULL;
      for (const Type *p = type; p->kind == TY_PTR; p = p->pa.ptrof) {
        const Type *ptrof = p->pa.ptrof;
        if (ptrof->kind == TY_FUNC || ptrof->kind == TY_ARRAY) {
          nestedtype = ptrof;
          break;
        }
      }
      if (nestedtype != NULL) {
        PrintTypeChain last = {
          NULL,
          print_nested_ptr_type2,
          NULL,
        };
        if (parent != NULL) {
          for (PrintTypeChain *p = parent;; p = p->parent) {
            if (p->parent == NULL) {
              p->parent = &last;
              break;
            }
          }
        } else {
          parent = &last;
        }

        PrintTypeChain chain = {
          parent,
          print_nested_ptr_type,
          type,
        };
        switch (nestedtype->kind) {
        case TY_FUNC:
          print_type_recur(fp, nestedtype->func.ret, &chain);
          print_func_params(fp, nestedtype);
          break;
        case TY_ARRAY:
          print_type_recur(fp, nestedtype->pa.ptrof, &chain);
          print_array_type(fp, nestedtype);
          break;
        default: assert(false); break;
        }
      } else {
        PrintTypeChain chain = {
          parent,
          print_ptr_type,
          type,
        };
        print_type_recur(fp, type->pa.ptrof, &chain);
      }
    }
    break;
  case TY_ARRAY:
    {
      PrintTypeChain chain = {
        parent,
        print_array_type,
        type,
      };
      const Type *nonarray;
      for (nonarray = type; nonarray->kind == TY_ARRAY; nonarray = nonarray->pa.ptrof)
        ;
      print_type_recur(fp, nonarray, &chain);
    }
    break;
  case TY_FUNC:
    {
      // No parenthesis.
      PrintTypeChain chain = {
        parent,
        print_func_params,
        type,
      };
      print_type_recur(fp, type->func.ret, &chain);
    }
    break;
  case TY_STRUCT:
    if (type->struct_.name != NULL) {
      fprintf(fp, "struct %.*s", type->struct_.name->bytes, type->struct_.name->chars);
    } else {
      fprintf(fp, "struct (anonymous)");
    }
    call_print_type_chain(parent, fp);
    break;
  }
}

void print_type(FILE *fp, const Type *type) {
  print_type_recur(fp, type, NULL);
}
