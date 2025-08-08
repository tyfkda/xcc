#include "../config.h"
#include "cc_misc.h"

#include <assert.h>
#include <inttypes.h>  // PRId64
#include <limits.h>  // INT_MAX
#include <stdlib.h>  // realloc
#include <string.h>  // memcpy

#include "ast.h"
#include "fe_misc.h"
#include "initializer.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

bool is_function_omitted(const VarInfo *funcvi) {
  assert(funcvi != NULL);
  int storage = funcvi->storage;
  if ((storage & VS_REF_TAKEN) || ((storage & (VS_INLINE | VS_EXTERN)) == (VS_INLINE | VS_EXTERN)))
    return false;
  return (satisfy_inline_criteria(funcvi) ||
      (storage & (VS_STATIC | VS_USED)) == VS_STATIC);  // Static function but not used.
}

static void eval_initial_value(Expr *expr, Expr **pvar, int64_t *poffset) {
  switch (expr->kind) {
  case EX_FIXNUM:
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
      int64_t offset1 = 0, offset2 = 0;
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
    break;
  case EX_MEMBER:
    {
      eval_initial_value(expr->member.target, pvar, poffset);
      const MemberInfo *minfo = expr->member.info;
      *poffset += minfo->offset;
    }
    break;
  case EX_COMPLIT:
    assert(expr->complit.var->kind == EX_VAR);
    eval_initial_value(expr->complit.var, pvar, poffset);
    break;
  // case EX_STR:  // should be handled in parser.
  default: assert(!"illegal"); break;
  }
}

#ifndef __NO_BITFIELD
static Fixnum calc_bitfield_initial_value(const StructInfo *sinfo, const Initializer *init,
                                          int *pi) {
  assert(init == NULL || (init->kind == IK_MULTI && init->multi->len == sinfo->member_count));
  Fixnum x = 0;
  int i = *pi, n = sinfo->member_count;
  for (bool top = true; i < n; ++i, top = false) {
    if (sinfo->is_union && !top)
      break;

    const MemberInfo *member = &sinfo->members[i];
    if (member->bitfield.width <= 0 || (!top && member->bitfield.position == 0))
      break;

    if (init == NULL)
      continue;
    const Initializer *mem_init = init->multi->data[i];
    if (mem_init == NULL) {
      if (sinfo->is_union) {
        // No initializer for bitfield member.
        *pi = -1;
        return 0;
      }
      continue;  // 0
    }
    assert(mem_init->kind == IK_SINGLE && mem_init->single->kind == EX_FIXNUM);

    Fixnum mask = (1LL << member->bitfield.width) - 1;
    x |= (mem_init->single->fixnum & mask) << member->bitfield.position;
  }
  *pi = i - 1;
  return x;
}

static int construct_initial_value_bitfield(
    const StructInfo *sinfo, const Initializer *init, int start, int *poffset,
    const ConstructInitialValueVTable *vtable, void *ud) {
  const MemberInfo *member = &sinfo->members[start];
  if (member->bitfield.width == 0)
    return start;

  int i = start;
  Fixnum x = calc_bitfield_initial_value(sinfo, init, &i);
  if (i < 0) {
    assert(sinfo->is_union);
    return start;
  }

  const Type *et = get_fixnum_type(member->bitfield.base_kind, false, 0);
  int offset = *poffset;
  int align = align_size(et);
  if (offset % align != 0) {
    (*vtable->emit_align)(ud, align);
    offset = ALIGN(offset, align);
  }

  (*vtable->emit_number)(ud, et, NULL, x);
  *poffset = offset += type_size(et);

  return i;
}
#endif

void construct_initial_value(const Type *type, const Initializer *init,
                             const ConstructInitialValueVTable *vtable, void *ud) {
  assert(init == NULL || init->kind != IK_DOT);

  switch (type->kind) {
  case TY_FLONUM:
#ifndef __NO_FLONUM
    switch (type->flonum.kind) {
    case FL_DOUBLE:
    case FL_LDOUBLE:  // long-double in XCC is same as double.
      {
        union {double f; uint64_t h;} v;
        v.f = 0;
        if (init != NULL) {
          assert(init->kind == IK_SINGLE);
          Expr *value = init->single;
          if (!(is_const(value) && is_flonum(value->type)))
            error("Illegal initializer: constant number expected");
          v.f = value->flonum;
        }
#if 0
        _DOUBLE(FLONUM(v.d));
#else
        (*vtable->emit_number)(ud, type, NULL, v.h);
#endif
      }
      break;
    case FL_FLOAT:
      {
        union {float f; uint32_t h;} v;
        v.f = 0;
        if (init != NULL) {
          assert(init->kind == IK_SINGLE);
          Expr *value = init->single;
          if (!(is_const(value) && is_flonum(value->type)))
            error("Illegal initializer: constant number expected");
          v.f = value->flonum;
        }
#if 0
        _FLOAT(FLONUM(v.f));
#else
        (*vtable->emit_number)(ud, type, NULL, v.h);
#endif
      }
      break;
    }
#else
    assert(false);
#endif
    break;
  case TY_FIXNUM:
  case TY_PTR:
    {
      Expr *var = NULL;
      Fixnum offset = 0;
      if (init != NULL) {
        assert(init->kind == IK_SINGLE);
        eval_initial_value(init->single, &var, &offset);
      }

      (*vtable->emit_number)(ud, type, var, offset);
    }
    break;
  case TY_ARRAY:
    if (init == NULL || init->kind == IK_MULTI) {
      const Type *elem_type = type->pa.ptrof;
      ssize_t index = 0;
      if (init != NULL) {
        Vector *init_array = init->multi;
        for (ssize_t i = 0; i < init_array->len; ++i, ++index) {
          const Initializer *init_elem = init_array->data[i];
          construct_initial_value(elem_type, init_elem, vtable, ud);
        }
      }
      // Padding
      for (ssize_t i = index, n = type->pa.length; i < n; ++i)
        construct_initial_value(elem_type, NULL, vtable, ud);
      break;
    }
    if (init->kind == IK_SINGLE) {
      Expr *e = strip_cast(init->single);
      if (e->kind == EX_STR && is_char_type(type->pa.ptrof, e->str.kind)) {
        assert(vtable->emit_string != NULL);
        (*vtable->emit_string)(ud, e, type_size(type));
        break;
      }
    }
    error("Illegal initializer");
    break;
  case TY_STRUCT:
    {
      const StructInfo *sinfo = type->struct_.info;
      assert(init == NULL || (init->kind == IK_MULTI && init->multi->len == sinfo->member_count));
      int count = 0;
      int offset = 0;
      for (int i = 0, n = sinfo->member_count; i < n; ++i) {
        const MemberInfo *member = &sinfo->members[i];
#ifndef __NO_BITFIELD
        if (member->bitfield.active) {
          i = construct_initial_value_bitfield(sinfo, init, i, &offset, vtable, ud);
          ++count;
          continue;
        }
#endif
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
          if (offset % align != 0) {
            assert(vtable->emit_align != NULL);
            (*vtable->emit_align)(ud, align);
            offset = ALIGN(offset, align);
          }
          construct_initial_value(member->type, mem_init, vtable, ud);
          ++count;
          offset += type_size(member->type);
        }
      }
      if (sinfo->is_union && count <= 0) {
        const MemberInfo *member = &sinfo->members[0];
        construct_initial_value(member->type, NULL, vtable, ud);
        offset += type_size(member->type);
      }

      size_t size = type_size(type);
      if (size != (size_t)offset) {
        // Put padding.
        int d = size - offset;
        const Type *type = get_fixnum_type_from_size(d);
        if (type != NULL) {
          (*vtable->emit_number)(ud, type, NULL, 0);
        } else {
          for (int i = 0; i < d; ++i)
            (*vtable->emit_number)(ud, &tyChar, NULL, 0);
        }
      }
    }
    break;
  case TY_FUNC: case TY_VOID: case TY_AUTO: assert(false); break;
  }
}

// Standard library doesn't have stable sort.

static void merge(char *base, size_t left, size_t mid, size_t right, size_t size,
                  int (*compare)(const void *, const void *), char *work) {
  size_t offset = left * size;
  char *dst = base + offset;
  char *lp = work + offset;
  char *lend = work + (mid + 1) * size;
  char *rp = lend;
  char *rend = work + (right + 1) * size;

  memcpy(lp, dst, rend - lp);

  for (;;) {
    if (compare(lp, rp) <= 0) {
      memcpy(dst, lp, size);
      dst += size;
      lp += size;
      if (lp >= lend)
        break;
    } else {
      memcpy(dst, rp, size);
      dst += size;
      rp += size;
      if (rp >= rend)
        break;
    }
  }

  if (lp < lend)
    memcpy(dst, lp, lend - lp);
  else if (rp < rend)
    memcpy(dst, rp, rend - rp);
}

static void merge_recur(char *base, size_t left, size_t right, size_t size,
                  int (*compare)(const void *, const void *), char *work) {
  if (left >= right)
    return;

  size_t mid = left + (right - left) / 2;
  merge_recur(base, left, mid, size, compare, work);
  merge_recur(base, mid + 1, right, size, compare, work);

  merge(base, left, mid, right, size, compare, work);
}

static int mymergesort(void *base, size_t nmemb, size_t size, int (*compare)(const void *, const void *)) {
  // char *work = malloc(size * nmemb);
  // if (work == NULL)
  //   return -ENOMEM;
  char *work = malloc_or_die(size * nmemb);
  merge_recur(base, 0, nmemb - 1, size, compare, work);
  free(work);
  return 0;
}

static void append_attr_func(AttrFuncContainer *container, Function *func, Vector *params,
                             bool for_dtor) {
  // Accept: `constructor`, `constructor(priority)`
  // Low value means high priority, negative value ignored.
  int priority = INT_MAX;
  if (params != NULL) {
    const Token *token;
    if (params->len != 1 || (token = params->data[0])->kind != TK_INTLIT ||
        token->fixnum.value < 0 || token->fixnum.value > 65535) {
      const char *cdtor = for_dtor ? "destructor" : "constructor";
      parse_error(PE_WARNING, func->ident, "Invalid priority for %s", cdtor);
    } else {
      priority = token->fixnum.value;
    }
  }

  int len = container->len;
  FuncAndPriority *data = realloc(container->data, (len + 1) * sizeof(*container->data));
  data[len].func = func;
  data[len].priority = priority;
  container->len = len + 1;
  container->data = data;
}

static int cmp_func_priority_ascending(const void *a, const void *b) {
  const FuncAndPriority *fpa = a, *fpb = b;
  return fpa->priority > fpb->priority;
}

static int cmp_func_priority_descending(const void *a, const void *b) {
  const FuncAndPriority *fpa = a, *fpb = b;
  return fpa->priority <= fpb->priority;
}

void sort_attr_func_container(AttrFuncContainer *container, bool ascending) {
  if (container == NULL || container->len <= 0)
    return;

  int (*cmp)(const void *, const void *) =
      ascending ? cmp_func_priority_ascending : cmp_func_priority_descending;

  // Use stable sort.
  mymergesort(&container->data[0], container->len, sizeof(FuncAndPriority), cmp);
}

void enumerate_ctor_dtors(Vector *decls, AttrFuncContainer *ctors, AttrFuncContainer *dtors) {
  const Name *constructor_name;
  const Name *destructor_name;

  if (ctors != NULL) {
    ctors->data = NULL;
    ctors->len = 0;
    constructor_name = alloc_name("constructor", NULL, false);
  }
  if (dtors != NULL) {
    dtors->data = NULL;
    dtors->len = 0;
    destructor_name = alloc_name("destructor", NULL, false);
  }
  for (int i = 0, len = decls->len; i < len; ++i) {
    Declaration *decl = decls->data[i];
    if (decl == NULL || decl->kind != DCL_DEFUN)
      continue;
    Function *func = decl->defun.func;
    if (func->attributes != NULL) {
      Vector *params;
      if (ctors != NULL && table_try_get(func->attributes, constructor_name, (void*)&params))
        append_attr_func(ctors, func, params, false);
      if (dtors != NULL && table_try_get(func->attributes, destructor_name, (void*)&params))
        append_attr_func(dtors, func, params, true);
    }
  }

  sort_attr_func_container(ctors, true);
  sort_attr_func_container(dtors, false);
}
