#include "../../config.h"
#include "initializer.h"

#include <assert.h>
#include <stdbool.h>

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
      is_str = is_char_type(type->pa.ptrof, single->str.kind);
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
      arr_len = init->single->str.len;
    } else {
      ssize_t index = 0;
      ssize_t max_index = 0;
      for (ssize_t i = 0; i < init->multi->len; ++i) {
        Initializer *init_elem = init->multi->data[i];
        if (init_elem != NULL && init_elem->kind == IK_BRKT) {
          index = init_elem->bracket.index;
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
    ssize_t init_len = is_str ? (ssize_t)init->single->str.len : (ssize_t)init->multi->len;
    if (init_len > arr_len && (!is_str || init_len - 1 > arr_len))  // Allow non-nul string.
      parse_error(PE_NOFATAL, NULL, "Initializer more than array size");
    return type;
  }
}

// Returns created global variable info.
static VarInfo *str_to_char_array(Scope *scope, Type *type, Initializer *init) {
  assert(init->kind == IK_SINGLE && init->single->kind == EX_STR);
  assert(type->kind == TY_ARRAY && is_char_type(type->pa.ptrof, init->single->str.kind));
  type = qualified_type(type, TQ_FORSTRLITERAL);
  const Token *ident = alloc_dummy_ident();
  VarInfo *varinfo = add_var_to_scope(scope, ident, type, VS_STATIC | VS_USED);
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
  assert(dst->type->kind == TY_ARRAY && is_char_type(dst->type->pa.ptrof, str->str.kind));

  ssize_t len = str->str.len;
  ssize_t dstlen = dst->type->pa.length;
  if (dstlen == -1) {
    dst->type->pa.length = dstlen = len;
  } else {
    if (dstlen < len - 1)
      parse_error(PE_FATAL, NULL, "Buffer is shorter than string: %d for \"%s\"", (int)dstlen,
                  str);
  }

  Type *strtype = dst->type;
  VarInfo *varinfo = str_to_char_array(curscope, strtype, src);
  Expr *var = new_expr_variable(varinfo->name, strtype, NULL, curscope);
  assert(str->type->kind == TY_ARRAY);
  return build_memcpy(dst, var, len * type_size(str->type->pa.ptrof));
}

typedef struct {
  Type *root_type;
  Vector *indices;
  Initializer *flattened;
} InitFlattener;

extern inline bool is_multi_type(enum TypeKind kind) {
  return kind == TY_STRUCT || kind == TY_ARRAY;
}

static Initializer *reserve_init_for_multi(Type *type, const Token *token) {
  int n;
  switch (type->kind) {
  case TY_STRUCT:
    {
      const StructInfo *sinfo = type->struct_.info;
      assert(sinfo != NULL);
      n = sinfo->member_count;
    }
    break;
  case TY_ARRAY:
    n = type->pa.length;
    if (n < 0) {
      // If n < 0 ([], FAM), then the array length is expanded according to initializer.
      // Make empty here.
      // If the array is VLA, initializer is prohibited, so safely ignored.
      n = 0;
    }
    break;
  default: assert(false); return NULL;
  }

  Vector *multi = new_vector();
  for (int i = 0; i < n; ++i)
    vec_push(multi, NULL);
  Initializer *init = new_initializer(IK_MULTI, token);
  init->multi = multi;
  return init;
}

static Initializer *flatten_initializer_multi(Type *type, Initializer *init);

static Type *get_multi_child_type(Type *type, int index) {
  switch (type->kind) {
  case TY_ARRAY:
    assert(index >= 0 && (type->pa.length < 0 || index < type->pa.length));
    return type->pa.ptrof;
  case TY_STRUCT:
    {
      const StructInfo *sinfo = type->struct_.info;
      assert(sinfo != NULL);
      assert(index >= 0 && index < sinfo->member_count);
      return sinfo->members[index].type;
    }
  default: assert(false); return NULL;
  }
}

static Initializer *find_next_indices(InitFlattener *flattener, Initializer *elem_init) {
  Vector *indices = flattener->indices;
  for (;;) {
    int depth = indices->len;
    assert(depth > 0);
    intptr_t last_index = VOIDP2INT(indices->data[depth - 1]) + 1;
    indices->data[depth - 1] = INT2VOIDP(last_index);

    Type *type = flattener->root_type;
    for (int i = 0; i < depth - 1; ++i) {
      type = get_multi_child_type(type, VOIDP2INT(indices->data[i]));
    }

    for (bool upward = false;;) {
      switch (type->kind) {
      case TY_ARRAY:
        if (type->pa.length >= 0) {
          if (last_index < type->pa.length) {
            type = type->pa.ptrof;
          } else {
            upward = true;
          }
        } else {
          assert(type->pa.length != LEN_VLA);
          // Target array's length is determined by initializer, so able to add new element.
          type = type->pa.ptrof;
        }
        break;
      case TY_STRUCT:
        {
          const StructInfo *sinfo = type->struct_.info;
          assert(sinfo != NULL);
          int member_count = sinfo->is_union ? 1 : sinfo->member_count;
          if (last_index < member_count) {
            type = sinfo->members[last_index].type;
          } else {
            upward = true;
          }
        }
        break;
      default: assert(false); break;
      }
      if (upward)
        break;

      if (!is_multi_type(type->kind)) {
        // Found the destination to store.
        return flatten_initializer(type, elem_init);
      }

      if (elem_init != NULL) {
        switch (elem_init->kind) {
        case IK_SINGLE:
          // Special handling for string.
          if (type->kind == TY_ARRAY) {
            if (elem_init->single->kind == EX_STR && is_char_type(type->pa.ptrof, elem_init->single->str.kind)) {
              return elem_init;
            }
          }
          if (is_multi_type(elem_init->single->type->kind)) {
            // In the case that variable or funcall is used as initializer.
            if (same_type_without_qualifier(type, elem_init->single->type, true))
              return elem_init;
          }
          break;
        case IK_MULTI:
          return flatten_initializer_multi(type, elem_init);
        default: assert(false); break;
        }
      }

      // Dig.
      vec_push(indices, INT2VOIDP(last_index = 0));
    }

    // Overflow elements: Move upward and continue.
    if (indices->len <= 1) {
      // No more elements to store.
      parse_error(PE_NOFATAL, elem_init->token, "Excess elements in array initializer");
      return NULL;
    }
    vec_pop(indices);
  }
}

static Initializer *find_desig_indices(InitFlattener *flattener, Initializer *init) {
  Vector *indices = flattener->indices;
  vec_clear(indices);

  Type *type = flattener->root_type;

  for (;;) {
    ssize_t index;
    switch (init->kind) {
    case IK_BRKT:
      if (type->kind != TY_ARRAY) {
        parse_error(PE_NOFATAL, init->token, "Illegal bracket designator");
        return NULL;
      } else {
        index = init->bracket.index;
        init = init->bracket.value;
        type = type->pa.ptrof;
      }
      break;
    case IK_DOT:
      if (type->kind != TY_STRUCT) {
        parse_error(PE_NOFATAL, init->token, "Illegal dotted designator");
        return NULL;
      } else {
        StructInfo *sinfo = type->struct_.info;
        assert(sinfo != NULL);
        const Name *name = init->dot.name;
        index = find_struct_member(sinfo, name);
        if (index < 0) {
          Vector *stack = new_vector();
          if (search_from_anonymous(type, name, NULL, stack) == NULL) {
            parse_error(PE_NOFATAL, init->token, "`%.*s' is not member of struct", NAMES(name));
            return NULL;
          }

          for (int i = 0; ;) {
            intptr_t subindex = VOIDP2INT(stack->data[i]);
            vec_push(indices, INT2VOIDP(subindex));
            const MemberInfo *minfo = &sinfo->members[subindex];
            type = minfo->type;
            if (++i >= stack->len)
              break;
            assert(type->kind == TY_STRUCT);
            sinfo = type->struct_.info;
            assert(sinfo != NULL);
          }
          init = init->dot.value;
          continue;
        }

        init = init->dot.value;
        type = sinfo->members[index].type;
      }
      break;
    case IK_MULTI:
      if (is_multi_type(type->kind))
        init = flatten_initializer_multi(type, init);
      return init;
    case IK_SINGLE:
      if (is_multi_type(type->kind) && !is_multi_type(init->single->type->kind)) {
        // To search first element of `type`, dig indices.
        vec_push(indices, INT2VOIDP(-1));
        return find_next_indices(flattener, init);
      }
      return flatten_initializer(type, init);
    default: assert(false); return NULL;
    }
    vec_push(indices, INT2VOIDP(index));
  }
}

static void store_to_current_position(InitFlattener *flattener, Initializer *init) {
  Vector *indices = flattener->indices;
  Type *type = flattener->root_type;
  Initializer *target = flattener->flattened;
  assert(target->kind == IK_MULTI);
  int depth = indices->len;
  for (int d = 0; ; ++d) {
    intptr_t index = VOIDP2INT(indices->data[d]);
    assert(index >= 0);
    if (index >= target->multi->len) {
      assert(type->kind == TY_ARRAY &&
             (type->pa.length == LEN_UND || type->pa.length == LEN_FAM));
      Vector *multi = target->multi;
      while (index >= multi->len)
        vec_push(multi, NULL);
    }
    if (d >= depth - 1) {
      Initializer **pp = (Initializer**)&target->multi->data[index];
      if (*pp != NULL)
        parse_error(PE_WARNING, init->token, "Initializer overlapped");
      *pp = init;
      return;
    }

    Type *elem_type = get_multi_child_type(type, index);
    Initializer *elem_init = target->multi->data[index];
    if (elem_init == NULL) {
      assert(is_multi_type(elem_type->kind));
      elem_init = reserve_init_for_multi(elem_type, init->token);
      target->multi->data[index] = elem_init;
    }
    if (elem_init->kind != IK_MULTI) {
      parse_error(PE_WARNING, init->token, "Initializer overlapped");
      elem_init = reserve_init_for_multi(elem_type, init->token);
      target->multi->data[index] = elem_init;
    }
    type = elem_type;
    target = elem_init;
  }
}

// Match multi initializer (`{...}`) to multi type (array or struct).
static Initializer *flatten_initializer_multi(Type *type, Initializer *init) {
  assert(is_multi_type(type->kind));
  assert(init->kind == IK_MULTI);
  if (type->kind == TY_ARRAY) {
    if (init->multi->len == 1) {
      Initializer *elem_init = init->multi->data[0];
      if (elem_init->kind == IK_SINGLE && elem_init->single->kind == EX_STR &&
          is_char_type(type->pa.ptrof, elem_init->single->str.kind))
        return elem_init;
    }
  }

  Vector *indices = new_vector();
  vec_push(indices, INT2VOIDP(-1));

  InitFlattener flattener = {
    .root_type = type,
    .indices = indices,
    .flattened = reserve_init_for_multi(type, init->token),
  };

  assert(is_multi_type(type->kind));
  for (int i = 0; i < init->multi->len; ++i) {
    Initializer *elem_init = init->multi->data[i];
    if (elem_init != NULL && (elem_init->kind == IK_BRKT || elem_init->kind == IK_DOT)) {
      // Search designated element and update indices.
      elem_init = find_desig_indices(&flattener, elem_init);
    } else {
      // Increment indices.
      elem_init = find_next_indices(&flattener, elem_init);
    }

    if (elem_init != NULL)
      store_to_current_position(&flattener, elem_init);
  }

  return flattener.flattened;
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

  if (is_multi_type(type->kind)) {
    if (init->kind == IK_MULTI)
      return flatten_initializer_multi(type, init);

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
    default: assert(false); break;
    }
  } else {
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

      if (!is_global_scope(scope) && !(varinfo->storage & VS_STATIC) &&
          get_callee_type(value->type) == NULL) {
        parse_error(PE_NOFATAL, value->token, "Allowed global reference only");
        return NULL;
      }
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
    if (value->token->kind != TK_DOT) {
      parse_error(PE_NOFATAL, value->token, "Allowed global reference only");
      return NULL;
    }
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
          if (eleminit != NULL && eleminit->kind == IK_BRKT) {
            eleminit->bracket.value = check_global_initializer(elemtype, eleminit->bracket.value);
          } else {
            multi->data[i] = check_global_initializer(elemtype, eleminit);
          }
        }
      }
      break;
    case IK_SINGLE:
      {
        Expr *e = strip_cast(init->single);
        if (e->kind == EX_STR && is_char_type(type->pa.ptrof, e->str.kind)) {
          assert(type->pa.length > 0);
          if ((ssize_t)e->str.len - 1 > type->pa.length) {  // Allow non-nul string.
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
          if (init_elem != NULL && init_elem->kind == IK_BRKT) {
            index = init_elem->bracket.index;
            init_elem = init_elem->bracket.value;
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
      if (init->single->kind == EX_STR &&
          is_char_type(expr->type->pa.ptrof, init->single->str.kind) ) {
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

void construct_initializing_stmts(Vector *decls) {
  for (int i = 0; i < decls->len; ++i) {
    VarDecl *decl = decls->data[i];
    if (decl->ident == NULL)
      continue;

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
  if (!(storage & VS_EXTERN) &&
      !ensure_struct(type, ident, curscope))
    return NULL;
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

      Expr *assign_sizevar = NULL;
      Expr *size_var;
      if (type->pa.size_var != NULL) {
        // `size_varname` for typedef'd type is already assigned.
        size_var = type->pa.size_var;
      } else {
        Expr *e = assign_sizevar = reserve_vla_type_size(type);
        while (e->kind == EX_COMMA)
          e = e->bop.rhs;
        assert(e->kind == EX_ASSIGN);
        size_var = e->bop.lhs;
      }
      assert(size_var->kind == EX_VAR);

      // Transform VLA to `alloca(size_var)`.
      const Token *tok = ident;
      Vector *params = new_vector();
      vec_push(params, &tySize);
      Type *functype = new_func_type(&tyVoidPtr, params, false);
      Expr *alloca_var = new_expr_variable(alloc_name("alloca", NULL, false), functype, tok,
                                           global_scope);
      Vector *args = new_vector();
      vec_push(args, size_var);
      Expr *call_alloca = new_expr_funcall(tok, alloca_var, args);
      if (assign_sizevar != NULL)
        call_alloca = new_expr_bop(EX_COMMA, &tyVoidPtr, tok, assign_sizevar, call_alloca);

      init = new_initializer(IK_SINGLE, tok);
      init->single = make_cast(type, tok, call_alloca, false);
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
    init = check_global_initializer(type, init);
  }
  return init;
}
