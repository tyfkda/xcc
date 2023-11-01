#include "../../config.h"
#include "initializer.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // qsort

#include "fe_misc.h"
#include "lexer.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

Type *fix_array_size(Type *type, Initializer *init) {
  assert(init != NULL);
  assert(type->kind == TY_ARRAY);

  ssize_t arr_len = type->pa.length;

  bool is_str = false;
  if (init->kind == IK_SINGLE) {
    Expr *single = init->single;
    switch (single->kind) {
    case EX_STR:
      is_str = is_char_type(type->pa.ptrof);
      break;
    case EX_COMPLIT:
      assert(single->type->kind == TY_ARRAY);
      if (arr_len == -1) {
        type = single->type;
      } else {
        if (single->type->pa.length != arr_len)
          parse_error(PE_NOFATAL, NULL, "Array length different");
      }
      return type;
    default: break;
    }
  }

  if (!is_str && init->kind != IK_MULTI) {
    // Error will be reported in another place.
    return type;
  }

  if (arr_len == -1) {
    if (is_str) {
      arr_len = init->single->str.size;
    } else {
      ssize_t index = 0;
      ssize_t max_index = 0;
      for (ssize_t i = 0; i < init->multi->len; ++i) {
        Initializer *init_elem = init->multi->data[i];
        if (init_elem->kind == IK_ARR) {
          index = init_elem->arr.index;
        }
        ++index;
        if (max_index < index)
          max_index = index;
      }
      arr_len = max_index;
    }
    Type *cloned = clone_type(type);
    cloned->pa.length = arr_len;
    return cloned;
  } else {
    assert(arr_len > 0);
    assert(!is_str || init->single->kind == EX_STR);
    ssize_t init_len = is_str ? (ssize_t)init->single->str.size : (ssize_t)init->multi->len;
    if (init_len > arr_len && (!is_str || init_len - 1 > arr_len))  // Allow non-nul string.
      parse_error(PE_NOFATAL, NULL, "Initializer more than array size");
    return type;
  }
}

// Returns created global variable info.
static VarInfo *str_to_char_array(Scope *scope, Type *type, Initializer *init) {
  assert(type->kind == TY_ARRAY && is_char_type(type->pa.ptrof));
  const Token *ident = alloc_dummy_ident();
  VarInfo *varinfo = add_var_to_scope(scope, ident, type, VS_STATIC);
  if (is_global_scope(scope))
    varinfo->global.init = init;
  else
    varinfo->static_.gvar->global.init = init;
  return varinfo;
}

Expr *str_to_char_array_var(Scope *scope, Expr *str) {
  Expr *s = strip_cast(str);
  if (s->kind != EX_STR)
    return str;
  if (str->kind == EX_CAST)
    return new_expr_cast(str->type, str->token, str_to_char_array_var(scope, str->unary.sub));

  Type *type = str->type;
  Initializer *init = new_initializer(IK_SINGLE, str->token);
  init->single = str;

  VarInfo *varinfo = str_to_char_array(scope, type, init);
  return new_expr_variable(varinfo->name, type, str->token, scope);
}

static Stmt *build_memcpy(Expr *dst, Expr *src, size_t size) {
  assert(!is_global_scope(curscope));
  Type *charptr_type = ptrof(&tyChar);
  Expr *dstexpr = alloc_tmp_var(curscope, charptr_type);
  Expr *srcexpr = alloc_tmp_var(curscope, charptr_type);
  Expr *sizeexpr = alloc_tmp_var(curscope, &tySize);

  Fixnum size_num_lit = size;
  Expr *size_num = new_expr_fixlit(&tySize, NULL, size_num_lit);

  Fixnum zero = 0;
  Expr *zeroexpr = new_expr_fixlit(&tySize, NULL, zero);

  Vector *stmts = new_vector();
  vec_push(stmts, new_stmt_expr(new_expr_bop(EX_ASSIGN, charptr_type, NULL, dstexpr, dst)));
  vec_push(stmts, new_stmt_expr(new_expr_bop(EX_ASSIGN, charptr_type, NULL, srcexpr, src)));
  vec_push(stmts, new_stmt_for(
      NULL,
      new_expr_bop(EX_ASSIGN, &tySize, NULL, sizeexpr, size_num),    // for (_size = size;
      new_expr_bop(EX_GT, &tyBool, NULL, sizeexpr, zeroexpr),        //      _size > 0;
      new_expr_unary(EX_PREDEC, sizeexpr->type, NULL, sizeexpr),     //      --_size)
      new_stmt_expr(                                                 //   *_dst++ = *_src++;
          new_expr_bop(EX_ASSIGN, &tyChar, NULL,
                       new_expr_unary(EX_DEREF, &tyChar, NULL,
                                      new_expr_unary(EX_POSTINC, dstexpr->type, NULL, dstexpr)),
                       new_expr_unary(EX_DEREF, &tyChar, NULL,
                                      new_expr_unary(EX_POSTINC, srcexpr->type, NULL, srcexpr))))));
  return new_stmt_block(NULL, stmts, NULL, NULL);
}

static Stmt *init_char_array_by_string(Expr *dst, Initializer *src) {
  // Initialize char[] with string literal (char s[] = "foo";).
  assert(src->kind == IK_SINGLE);
  const Expr *str = src->single;
  assert(str->kind == EX_STR);
  assert(dst->type->kind == TY_ARRAY && is_char_type(dst->type->pa.ptrof));

  ssize_t size = str->str.size;
  ssize_t dstsize = dst->type->pa.length;
  if (dstsize == -1) {
    dst->type->pa.length = dstsize = size;
  } else {
    if (dstsize < size - 1)
      parse_error(PE_FATAL, NULL, "Buffer is shorter than string: %d for \"%s\"", (int)dstsize,
                  str);
  }

  Type *strtype = dst->type;
  VarInfo *varinfo = str_to_char_array(curscope, strtype, src);
  Expr *var = new_expr_variable(varinfo->name, strtype, NULL, curscope);
  return build_memcpy(dst, var, size);
}

static int compare_desig_start(const void *a, const void *b) {
  const ssize_t *pa = *(ssize_t**)a;
  const ssize_t *pb = *(ssize_t**)b;
  ssize_t d = *pa - *pb;
  return d > 0 ? 1 : d < 0 ? -1 : 0;
}

static Initializer *flatten_array_initializer(Initializer *init) {
  // Check whether IK_DOT or IK_ARR exists.
  int i = 0, len = init->multi->len;
  for (; i < len; ++i) {
    Initializer *init_elem = init->multi->data[i];
    if (init_elem->kind == IK_DOT)
      parse_error(PE_FATAL, NULL, "dot initializer for array");
    if (init_elem->kind == IK_ARR)
      break;
  }
  if (i >= len)  // IK_ARR not exits.
    return init;

  // Enumerate designated initializer.
  Vector *ranges = new_vector();  // <(start, count)>
  size_t lastStartIndex = 0;
  size_t lastStart = 0;
  size_t index = i;
  for (; i <= len; ++i, ++index) {  // '+1' is for last range.
    Initializer *init_elem = NULL;
    if (i >= len || (init_elem = init->multi->data[i])->kind == IK_ARR) {
      if ((size_t)i > lastStartIndex) {
        size_t *range = malloc_or_die(sizeof(size_t) * 3);
        range[0] = lastStart;
        range[1] = lastStartIndex;
        range[2] = index - lastStart;
        vec_push(ranges, range);
      }
      if (i >= len)
        break;
      lastStart = index = init_elem->arr.index;
      lastStartIndex = i;
    } else if (init_elem->kind == IK_DOT)
      parse_error(PE_FATAL, NULL, "dot initializer for array");
  }

  // Sort
  qsort(ranges->data, ranges->len, sizeof(size_t *), compare_desig_start);

  // Reorder
  Vector *reordered = new_vector();
  index = 0;
  for (int i = 0; i < ranges->len; ++i) {
    size_t *p = ranges->data[i];
    size_t start = p[0];
    size_t index = p[1];
    size_t count = p[2];
    if (i > 0) {
      size_t *q = ranges->data[i - 1];
      if (start < q[0] + q[2])
        parse_error(PE_FATAL, NULL, "Initializer for array overlapped");
    }
    for (size_t j = 0; j < count; ++j) {
      Initializer *elem = init->multi->data[index + j];
      if (j == 0 && index != start && elem->kind != IK_ARR) {
        Initializer *arr = new_initializer(IK_ARR, elem->token);
        arr->arr.index = start;
        arr->arr.value = elem;
        elem = arr;
      }
      vec_push(reordered, elem);
    }
  }

  Initializer *init2 = new_initializer(IK_MULTI, init->token);
  init2->multi = reordered;
  return init2;
}

static bool is_multi_type(enum TypeKind kind) {
  return kind == TY_STRUCT || kind == TY_ARRAY;
}

static Initializer *flatten_initializer_multi0(Type *type, Initializer *init);

static Initializer *flatten_initializer_multi(Type *type, Initializer *init, int *pindex) {
  assert(init != NULL);
  switch (init->kind) {
  case IK_SINGLE:
    *pindex += 1;
    return flatten_initializer(type, init);
  case IK_MULTI:
    break;
  default:
    parse_error(PE_NOFATAL, init->token, "Illegal initializer");
    return init;
  }

  if (*pindex < 0) {
    *pindex = 0;
  } else if (*pindex < init->multi->len) {
    Initializer *e = init->multi->data[*pindex];
    if (e->kind == IK_MULTI && is_multi_type(type->kind)) {
      *pindex += 1;
      return flatten_initializer_multi0(type, e);
    }
  }

  if (!is_multi_type(type->kind)) {
    Initializer *elem_init = init->multi->data[*pindex];
    *pindex += 1;
    return flatten_initializer(type, elem_init);
  }

  switch (type->kind) {
  case TY_STRUCT:
    {
      const StructInfo *sinfo = type->struct_.info;
      int n = sinfo->member_count;
      int m = init->multi->len;
      if (n <= 0) {
        return init;
      }

      if (*pindex < m) {
        Initializer *elem_init = init->multi->data[*pindex];
        if (elem_init != NULL && elem_init->kind == IK_SINGLE &&
            same_type_without_qualifier(type, elem_init->single->type, true)) {
          *pindex += 1;
          return elem_init;
        }
      }

      Initializer **values = malloc_or_die(sizeof(Initializer*) * n);
      for (int i = 0; i < n; ++i)
        values[i] = NULL;

      int midx = 0;
      while (*pindex < m) {
        Initializer *value = init->multi->data[*pindex];
        if (value == NULL) {
          *pindex += 1;
          continue;
        }

        if (value->kind == IK_ARR) {
          parse_error(PE_NOFATAL, value->token, "indexed initializer for struct");
          *pindex += 1;
          continue;
        }

        bool dot = value->kind == IK_DOT;
        if (dot) {
          const Name *name = value->dot.name;
          midx = find_struct_member(sinfo, name);
          if (midx >= 0) {
            value = value->dot.value;
          } else {
            Vector *stack = new_vector();
            if (search_from_anonymous(type, name, NULL, stack) == NULL) {
              parse_error(PE_NOFATAL, value->token, "`%.*s' is not member of struct", NAMES(name));
              *pindex += 1;
              continue;
            }

            midx = (intptr_t)stack->data[0];
            Vector *multi = new_vector();
            vec_push(multi, value);
            Initializer *init2 = new_initializer(IK_MULTI, value->token);
            init2->multi = multi;
            value = init2;
          }
        }

        if (midx >= n)
          break;

        MemberInfo *minfo = &sinfo->members[midx];
        if (dot) {
          value = flatten_initializer(minfo->type, value);
          *pindex += 1;
        } else {
          Type *mt = minfo->type;
          if (sinfo->is_flexible && midx == sinfo->member_count - 1) {
            // Special handling for flexible array member: allow arbitrary length of initializer.
            mt = clone_type(mt);
            assert(mt->kind == TY_ARRAY);
            mt->pa.length = -1;
          }
          value = flatten_initializer_multi(mt, init, pindex);
        }
        values[midx++] = value;

        if (sinfo->is_union)
          break;
      }

      Initializer *flat = new_initializer(IK_MULTI, init->token);
      Vector *v = new_vector();
      v->len = v->capacity = n;
      v->data = (void**)values;
      flat->multi = v;
      return flat;
    }
  case TY_ARRAY:
    {
      if (is_char_type(type->pa.ptrof) && *pindex < init->multi->len) {
        Initializer *elem_init = init->multi->data[*pindex];
        if (elem_init->kind == IK_SINGLE && elem_init->single->kind == EX_STR) {
          *pindex += 1;
          return elem_init;
        }
      }

      Type *elem_type = type->pa.ptrof;
      Vector *elems = new_vector();
      ssize_t eidx = 0;
      while (*pindex < init->multi->len) {
        Initializer *elem_init = init->multi->data[*pindex];
        if (elem_init->kind == IK_ARR) {
          elem_init->arr.value = flatten_initializer(elem_type, elem_init->arr.value);
          eidx = elem_init->arr.index;
          assert(type->pa.length < 0 || eidx < type->pa.length);
          *pindex += 1;
        } else {
          if (type->pa.length >= 0 && eidx >= type->pa.length)
            break;
          elem_init = flatten_initializer_multi(elem_type, init, pindex);
        }
        vec_push(elems, elem_init);
        ++eidx;
      }

      Initializer *init2 = new_initializer(IK_MULTI, init->token);
      init2->multi = elems;
      return init2;
    }
  default: assert(false); break;
  }
  return init;
}

static Initializer *flatten_initializer_multi0(Type *type, Initializer *init) {
  if (type->kind == TY_ARRAY)
    init = flatten_array_initializer(init);

  int index = -1;
  Initializer *flat = flatten_initializer_multi(type, init, &index);
  switch (type->kind) {
  case TY_ARRAY:
  case TY_STRUCT:
    if (index < init->multi->len) {
      const char *tstr = type->kind == TY_ARRAY ? "array" : "struct";
      parse_error(PE_WARNING, ((Initializer*)init->multi->data[index])->token,
                  "Excess elements in %s initializer", tstr);
    }
    break;
  default: assert(false); break;
  }
  return flat;
}

static void flatten_initializer_single(Expr *value) {
  switch (value->kind) {
  case EX_REF:
  case EX_DEREF:
  case EX_CAST:
    flatten_initializer_single(value->unary.sub);
    break;

  case EX_COMPLIT:
    {
      Initializer *init = flatten_initializer(value->type, value->complit.original_init);
      value->complit.original_init = init;

      Expr *var = value->complit.var;
      if (is_global_scope(var->var.scope)) {
        assert(init->kind == IK_MULTI);
        VarInfo *varinfo = scope_find(var->var.scope, var->var.name, NULL);
        assert(varinfo != NULL);
        varinfo->global.init = init;
      }
    }
    break;
  default:
    // TODO: Confirm.
    break;
  }
}

Initializer *flatten_initializer(Type *type, Initializer *init) {
  if (init == NULL)
    return NULL;

  if (init->kind == IK_MULTI && is_multi_type(type->kind))
    return flatten_initializer_multi0(type, init);

  switch (type->kind) {
  case TY_STRUCT:
    if (init->kind == IK_SINGLE) {
      Expr *e = init->single;
      if (!same_type_without_qualifier(type, e->type, true))
        parse_error(PE_NOFATAL, init->token, "Incompatible type");
      if (e->kind == EX_COMPLIT)
        flatten_initializer_single(e);
    }
    break;
  case TY_ARRAY:
    switch (init->kind) {
    case IK_SINGLE:
      // Special handling for string (char[]), and accept length difference.
      if (init->single->type->kind == TY_ARRAY &&
          can_cast(type->pa.ptrof, init->single->type->pa.ptrof, is_zero(init->single), false))
        break;
      // Fallthrough
    default:
      // Error will be reported in another place.
      break;
    }
    break;
  default:
    switch (init->kind) {
    case IK_MULTI:
      if (init->multi->len != 1 || ((Initializer*)init->multi->data[0])->kind != IK_SINGLE) {
        parse_error(PE_NOFATAL, init->token, "Requires scaler");
        break;
      }
      init = init->multi->data[0];
      assert(init->kind == IK_SINGLE);
      // Fallthrough
    case IK_SINGLE:
      flatten_initializer_single(init->single);
      break;
    default:
      parse_error(PE_NOFATAL, init->token, "Error initializer");
      // TODO: Modify init
      break;
    }
    break;
  }
  return init;
}

static Initializer *check_global_initializer(Type *type, Initializer *init);

static Expr *check_global_initializer_fixnum(Expr *value, bool *isconst) {
  switch (value->kind) {
  case EX_FIXNUM:
  case EX_FLONUM:
    *isconst = true;
    break;
  case EX_STR:
    // Create string and point to it.
    value = str_to_char_array_var(curscope, value);
    *isconst = true;
    break;
  case EX_VAR:
    {
      Scope *scope;
      VarInfo *varinfo = scope_find(value->var.scope, value->var.name, &scope);
      if (varinfo == NULL) {
        // Error must be raised already, so exit quietly.
        *isconst = true;
        break;
      }

      if (!is_global_scope(scope) && !(varinfo->storage & VS_STATIC))
        parse_error(PE_FATAL, value->token, "Allowed global reference only");
      *isconst = value->type->kind == TY_ARRAY || value->type->kind == TY_FUNC ||
                 (value->type->kind == TY_PTR && value->type->pa.ptrof->kind == TY_FUNC);
    }
    break;
  case EX_COMPLIT:
    value->complit.original_init = check_global_initializer(value->type,
                                                            value->complit.original_init);
    *isconst = value->type->kind == TY_ARRAY;
    break;
  case EX_ADD:
  case EX_SUB:
    {
      bool lhs_const = false, rhs_const = false;
      value->bop.lhs = check_global_initializer_fixnum(value->bop.lhs, &lhs_const);
      value->bop.rhs = check_global_initializer_fixnum(value->bop.rhs, &rhs_const);
      *isconst = lhs_const && rhs_const;
    }
    break;
  case EX_REF:
    value->unary.sub = check_global_initializer_fixnum(value->unary.sub, isconst);
    *isconst = true;
    break;
  case EX_DEREF:
  case EX_CAST:
    value->unary.sub = check_global_initializer_fixnum(value->unary.sub, isconst);
    break;
  case EX_MEMBER:
    value->member.target = check_global_initializer_fixnum(value->member.target, isconst);
    if (value->token->kind != TK_DOT)
      parse_error(PE_FATAL, value->token, "Allowed global reference only");
    *isconst = value->type->kind == TY_ARRAY;
    break;
  default:
    *isconst = false;
    break;
  }
  return value;
}

static Initializer *check_global_initializer(Type *type, Initializer *init) {
  if (init == NULL)
    return NULL;

  switch (type->kind) {
#ifndef __NO_FLONUM
  case TY_FLONUM:
    if (init->kind == IK_SINGLE) {
      switch (init->single->kind) {
      case EX_FIXNUM:
        {
          Fixnum fixnum = init->single->fixnum;
          init->single = new_expr_flolit(type, init->single->token, fixnum);
        }
        // Fallthrough
      case EX_FLONUM:
        return init;
      default:
        parse_error(PE_NOFATAL, init->single->token, "Constant expression expected");
        break;
      }
    }
    break;
#endif
  case TY_FIXNUM:
  case TY_PTR:
    {
      assert(init->kind == IK_SINGLE);
      bool isconst = false;
      Expr *value = check_global_initializer_fixnum(init->single, &isconst);
      init->single = make_cast(type, init->single->token, value, false);
      if (!isconst) {
        parse_error(PE_NOFATAL, init->single->token, "Initializer must be constant");
      }
    }
    break;
  case TY_ARRAY:
    if (init->kind == IK_SINGLE && init->single->kind == EX_COMPLIT)
      init = init->single->complit.original_init;
    switch (init->kind) {
    case IK_MULTI:
      {
        Type *elemtype = type->pa.ptrof;
        Vector *multi = init->multi;
        for (int i = 0, len = multi->len; i < len; ++i) {
          Initializer *eleminit = multi->data[i];
          if (eleminit->kind == IK_ARR) {
            eleminit->arr.value = check_global_initializer(elemtype, eleminit->arr.value);
          } else {
            multi->data[i] = check_global_initializer(elemtype, eleminit);
          }
        }
      }
      break;
    case IK_SINGLE:
      if (is_char_type(type->pa.ptrof)) {
        Expr *e = strip_cast(init->single);
        if (e->kind == EX_STR) {
          assert(type->pa.length > 0);
          if ((ssize_t)e->str.size - 1 > type->pa.length) {  // Allow non-nul string.
            parse_error(PE_NOFATAL, init->single->token, "Array size shorter than initializer");
          }
          break;
        }
      }
      // Fallthrough
    case IK_DOT:
    default:
      parse_error(PE_NOFATAL, init->token, "Array initializer requires `{'");
      break;
    }
    break;
  case TY_STRUCT:
    {
      if (init->kind == IK_SINGLE && init->single->kind == EX_COMPLIT)
        init = init->single->complit.original_init;
      if (init->kind != IK_MULTI) {
        parse_error(PE_NOFATAL, init->token, "Struct initializer requires `{'");
        return NULL;
      }
      const StructInfo *sinfo = type->struct_.info;
      for (int i = 0, n = sinfo->member_count; i < n; ++i) {
        const MemberInfo *member = &sinfo->members[i];
        Initializer *init_elem = init->multi->data[i];
        if (init_elem != NULL)
          init->multi->data[i] = check_global_initializer(member->type, init_elem);
      }
    }
    break;
  default:
    parse_error(PE_NOFATAL, NULL, "Global initial value for type %d not implemented (yet)\n",
                type->kind);
    break;
  }
  return init;
}

Vector *assign_initial_value(Expr *expr, Initializer *init, Vector *inits) {
  if (init == NULL)
    return inits;

  if (inits == NULL)
    inits = new_vector();

  switch (expr->type->kind) {
  case TY_ARRAY:
    switch (init->kind) {
    case IK_MULTI:
      {
        ssize_t arr_len = expr->type->pa.length;
        if (arr_len > 0 && init->multi->len > arr_len)
          parse_error(PE_FATAL, init->token, "Initializer more than array size");

        assert(!is_global_scope(curscope));
        Type *ptr_type = array_to_ptr(expr->type);
        Expr *ptr_var = alloc_tmp_var(curscope, ptr_type);
        vec_push(inits, new_stmt_expr(new_expr_bop(EX_ASSIGN, ptr_type, NULL, ptr_var, expr)));

        const size_t len = init->multi->len;
        const size_t elem_size = type_size(expr->type->pa.ptrof);
        size_t prev_index = 0, index = 0;
        for (size_t i = 0; i < len; ++i) {
          Initializer *init_elem = init->multi->data[i];
          if (init_elem->kind == IK_ARR) {
            index = init_elem->arr.index;
            init_elem = init_elem->arr.value;
          }

          size_t add = index - prev_index;
          if (add > 0) {
            const Fixnum n = add * elem_size;
            vec_push(inits, new_stmt_expr(
                new_expr_bop(EX_ASSIGN, ptr_type, NULL, ptr_var,
                    new_expr_bop(EX_ADD, ptr_type, NULL, ptr_var,
                        new_expr_fixlit(&tySize, NULL, n)))));
          }

          assign_initial_value(new_expr_deref(NULL, ptr_var), init_elem, inits);
          prev_index = index++;
        }
      }
      break;
    case IK_SINGLE:
      // Special handling for string (char[]).
      if (is_char_type(expr->type->pa.ptrof) &&
          init->single->kind == EX_STR) {
        vec_push(inits, init_char_array_by_string(expr, init));
        break;
      }
      if (init->kind == IK_SINGLE && init->single->kind == EX_COMPLIT) {
        Expr *single = init->single;
        if (!same_type_without_qualifier(expr->type, single->type, true)) {
          // Error should be raised before here.
          // parse_error(PE_NOFATAL, init->token, "Different type initializer");
        } else {
          vec_push(inits, build_memcpy(expr, single, type_size(expr->type)));
        }
        break;
      }
      // Fallthrough
    default:
      parse_error(PE_NOFATAL, init->token, "Array initializer requires `{'");
      break;
    }
    break;
  case TY_STRUCT:
    {
      if (init->kind == IK_SINGLE) {
        Expr *e = init->single;
        if (same_type_without_qualifier(expr->type, e->type, true)) {
          vec_push(inits, new_stmt_expr(new_expr_bop(EX_ASSIGN, expr->type, init->token, expr, e)));
          break;
        }
      }
      if (init->kind != IK_MULTI) {
        parse_error(PE_NOFATAL, init->token, "Struct initializer requires `{'");
        break;
      }

      const StructInfo *sinfo = expr->type->struct_.info;
      if (!sinfo->is_union) {
        Token *tok = alloc_token(TK_DOT, NULL, ".", NULL);
        for (int i = 0, n = sinfo->member_count; i < n; ++i) {
          Initializer *init_elem = init->multi->data[i];
          if (init_elem == NULL)
            continue;
          const MemberInfo *minfo = &sinfo->members[i];
          Expr *member = new_expr_member(tok, minfo->type, expr, NULL, minfo);
#ifndef __NO_BITFIELD
          if (minfo->bitfield.width > 0) {
            if (init_elem->kind != IK_SINGLE) {
              parse_error(PE_FATAL, init_elem->token, "illegal initializer for member `%.*s'",
                          NAMES(minfo->name));
            } else {
              vec_push(inits, new_stmt_expr(assign_to_bitfield(init_elem->token, member,
                                                               init_elem->single, minfo)));
            }
          } else
#endif
          {
            assign_initial_value(member, init_elem, inits);
          }
        }
      } else {
        int n = sinfo->member_count;
        int m = init->multi->len;
        if (n <= 0 && m > 0)
          parse_error(PE_FATAL, init->token, "Initializer for empty union");

        int count = 0;
        Token *tok = alloc_token(TK_DOT, NULL, ".", NULL);
        for (int i = 0; i < n; ++i) {
          Initializer *init_elem = init->multi->data[i];
          if (init_elem == NULL)
            continue;
          if (count > 0) {
            parse_error(PE_FATAL, init_elem->token, "More than one initializer for union");
            break;
          }

          const MemberInfo *minfo = &sinfo->members[i];
          Expr *mem = new_expr_member(tok, minfo->type, expr, NULL, minfo);
          assign_initial_value(mem, init_elem, inits);
          ++count;
        }
      }
    }
    break;
  default:
    {
      assert(init->kind == IK_SINGLE);
      Expr *value = str_to_char_array_var(curscope, init->single);
      vec_push(inits,
               new_stmt_expr(new_expr_bop(EX_ASSIGN, expr->type, init->token, expr,
                                          make_cast(expr->type, init->token, value, false))));
    }
    break;
  }

  return inits;
}

#ifndef __NO_BITFIELD
Fixnum calc_bitfield_initial_value(const StructInfo *sinfo, const Initializer *init, int *pi) {
  assert(!sinfo->is_union);  // TODO
  assert(init == NULL || (init->kind == IK_MULTI && init->multi->len == sinfo->member_count));
  Fixnum x = 0;
  int i = *pi, n = sinfo->member_count;
  for (bool top = true; i < n; ++i, top = false) {
    const MemberInfo *member = &sinfo->members[i];
    if (member->bitfield.width <= 0 || (!top && member->bitfield.position == 0))
      break;

    if (init == NULL)
      continue;
    const Initializer *mem_init = init->multi->data[i];
    if (mem_init == NULL)
      continue;  // 0
    assert(mem_init->kind == IK_SINGLE && mem_init->single->kind == EX_FIXNUM);

    Fixnum mask = (1LL << member->bitfield.width) - 1;
    x |= (mem_init->single->fixnum & mask) << member->bitfield.position;
  }
  *pi = i - 1;
  return x;
}
#endif

void construct_initializing_stmts(Vector *decls) {
  for (int i = 0; i < decls->len; ++i) {
    VarDecl *decl = decls->data[i];
    Scope *scope;
    VarInfo *varinfo = scope_find(curscope, decl->ident, &scope);
    assert(scope == curscope);
    if (varinfo->storage & (VS_STATIC | VS_EXTERN))
      continue;
    Expr *var = new_expr_variable(decl->ident, varinfo->type, NULL, curscope);
    assert(!is_global_scope(curscope));
    if (varinfo->local.init != NULL) {
      Vector *inits = assign_initial_value(var, varinfo->local.init, NULL);
      decl->init_stmt = new_stmt_block(NULL, inits, NULL, NULL);
      // varinfo->local.init = NULL;
    }
  }
}

Initializer *check_vardecl(Type **ptype, const Token *ident, int storage, Initializer *init) {
  if (storage & VS_EXTERN && init != NULL) {
    parse_error(PE_NOFATAL, init->token, "extern with initializer");
    return NULL;
  }

  Type *type = *ptype;
  if (!(storage & VS_EXTERN))
    ensure_struct(type, ident, curscope);
  init = flatten_initializer(type, init);
  if (type->kind == TY_ARRAY) {
    if (init != NULL) {
      *ptype = type = fix_array_size(type, init);
    } else if (type->pa.length == -1 && !(storage & VS_EXTERN)
#ifndef __NO_VLA
               && type->pa.vla == NULL
#endif
              ) {
      parse_error(PE_WARNING, ident, "Array size undetermined, assume as one");
      type->pa.length = 1;
    }
#ifndef __NO_VLA
  } else if (type->kind == TY_PTR && type->pa.vla != NULL) {
    if (is_global_scope(curscope) || (storage & VS_STATIC)) {
      parse_error(PE_NOFATAL, ident, "Variable length array cannot use in global scope");
      init = NULL;
    } else {
      if (init != NULL)
        parse_error(PE_NOFATAL, ident, "Variable length array with initializer");

      // Transform VLA to `alloca(vla * type_size(elem))`.
      const Token *tok = ident;
      Vector *params = new_vector();
      vec_push(params, &tySize);
      Type *functype = new_func_type(&tyVoidPtr, params, false);
      Expr *alloca_var = new_expr_variable(alloc_name("alloca", NULL, false), functype, tok,
                                           global_scope);
      Expr *size_expr = calc_type_size(type);
      Vector *args = new_vector();
      vec_push(args, size_expr);
      Expr *call_alloca = new_expr_funcall(tok, alloca_var, functype->func.ret, args);
      init = new_initializer(IK_SINGLE, tok);
      init->single = make_cast(ptrof(type->pa.ptrof), tok, call_alloca, false);
    }
#endif
  }

  if (curfunc != NULL) {
    VarInfo *varinfo = scope_find(curscope, ident->ident, NULL);
    varinfo->type = type;

    // TODO: Check `init` can be cast to `type`.
    if (storage & VS_STATIC) {
      VarInfo *gvarinfo = varinfo->static_.gvar;
      assert(gvarinfo != NULL);
      gvarinfo->global.init = init = check_global_initializer(type, init);
      gvarinfo->type = type;
      // static variable initializer is handled in codegen, same as global variable.
    } else {
      varinfo->local.init = init;
    }
  } else {
    VarInfo *gvarinfo = scope_find(global_scope, ident->ident, NULL);
    assert(gvarinfo != NULL);
    gvarinfo->global.init = init = check_global_initializer(type, init);
    gvarinfo->type = type;
  }
  return init;
}
