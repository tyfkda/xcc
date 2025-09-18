#include "../../config.h"
#include "expr.h"

#include <assert.h>

#include "fe_misc.h"
#include "initializer.h"
#include "type.h"
#include "util.h"
#include "var.h"

Expr *string_expr(const Token *token, char *str, ssize_t len, enum StrKind kind) {
  enum FixnumKind fxkind = FX_CHAR;
  bool is_unsigned = false;
#ifndef __NO_WCHAR
  switch (kind) {
  case STR_CHAR:  break;
  case STR_WIDE:  fxkind = FX_INT; is_unsigned = true; break;  // TODO: Match with wchar_t.
  }
#endif
  Type *ctype = get_fixnum_type(fxkind, is_unsigned, 0);  // not const.
  Type *type = arrayof(ctype, len);
  type->qualifier = TQ_CONST;

  Expr *expr = new_expr(EX_STR, type, token);
  expr->str.buf = str;
  expr->str.len = len;
  expr->str.kind = kind;
  return expr;
}

Expr *calc_type_size(const Type *type) {
#ifndef __NO_VLA
  if (ptr_or_array(type) && type->pa.vla != NULL) {
    assert(type->pa.size_var != NULL);
    return type->pa.size_var;
  }
#endif
  return new_expr_fixlit(&tySize, NULL, type_size(type));
}

#ifndef __NO_VLA
Expr *reserve_vla_type_size(Type *type) {
  assert(!is_global_scope(curscope));
  if (!ptr_or_array(type))
    return NULL;

  // If VLA is nested, calculate subtype first.
  Type *subtype = type->pa.ptrof;
  Expr *sub = reserve_vla_type_size(subtype);
  if (type->pa.vla == NULL)
    return sub;

  const Token *token = type->pa.vla->token;
  Expr *var = alloc_tmp_var(curscope, &tySize);
  Expr *value = new_expr_num_bop(EX_MUL, token, make_cast(&tySize, token, type->pa.vla, false),
                                 calc_type_size(type->pa.ptrof));
  Expr *assign = new_expr_bop(EX_ASSIGN, &tySize, token, var, value);
  type->pa.size_var = var;

  if (sub != NULL)
    assign = new_expr_bop(EX_COMMA, &tySize, assign->token, sub, assign);
  return assign;
}

Expr *calc_vla_size(Type *type) {
  for (;;) {
    if (ptr_or_array(type) && type->pa.vla != NULL) {
      if (type->pa.size_var != NULL)
        break;
      return reserve_vla_type_size(type);
    }

    if (type->kind != TY_PTR)
      break;
    type = type->pa.ptrof;
  }
  return NULL;
}
#endif

Expr *make_cast(Type *type, const Token *token, Expr *sub, bool is_explicit) {
  check_cast(type, sub->type, is_zero(sub), is_explicit, token);
  if (same_type(type, sub->type)) {
    sub->type = type;
    return sub;
  }

  if (sub->kind == EX_FIXNUM || sub->kind == EX_FLONUM) {
#ifndef __NO_FLONUM
    switch (sub->kind) {
    case EX_FLONUM:
      if (type->kind == TY_FIXNUM) {
        Fixnum fixnum;
        if (is_bool(type))
          fixnum = sub->flonum != 0;
        else
          fixnum = wrap_value(sub->flonum, type_size(type), type->fixnum.is_unsigned);
        return new_expr_fixlit(type, sub->token, fixnum);
      }
      assert(type->kind == TY_FLONUM);
      sub->type = type;
      return sub;
    case EX_FIXNUM:
      if (type->kind == TY_FLONUM) {
        Flonum flonum = (sub->type->kind != TY_FIXNUM || sub->type->fixnum.is_unsigned)
                            ? (Flonum)(UFixnum)sub->fixnum
                            : (Flonum)sub->fixnum;
        return new_expr_flolit(type, sub->token, flonum);
      }
      break;
    default:
      break;
    }
#endif

    assert(sub->kind == EX_FIXNUM);
    assert(!is_flonum(type));
    Fixnum value;
    if (is_bool(type))
      value = sub->fixnum != 0;
    else
      value = wrap_value(sub->fixnum, type_size(type), type->fixnum.is_unsigned);
    sub->fixnum = value;
    sub->type = type;
    return sub;
  }

  if (is_bool(type))
    return make_cond(sub);

#if XCC_TARGET_ARCH == XCC_ARCH_X64 && !defined(__NO_FLONUM)
  // On x64, cannot cast from double to uint64_t directly.
  size_t dst_size = type_size(type);
  if (is_flonum(sub->type) &&
      is_fixnum(type->kind) && type->fixnum.is_unsigned && dst_size >= 8) {
    // Transform from (uint64_t)flonum
    //   to: (flonum <= INT64_MAX) ? (int64_t)flonum
    //                             : ((int64_t)(flonum - (INT64_MAX + 1UL)) ^ (1L << 63))
    Type *i64t = get_fixnum_type_from_size(dst_size);
    Expr *cond = new_expr_bop(EX_LE, &tyBool, token, sub,
                              new_expr_flolit(sub->type, sub->token, INT64_MAX));
    Expr *offsetted = new_expr_addsub(
        EX_SUB, token, sub,
        new_expr_flolit(sub->type, sub->token, (uint64_t)INT64_MAX + 1UL));
    Expr *xorred = new_expr_bop(EX_BITXOR, i64t, token, make_cast(i64t, token, offsetted, false),
                                new_expr_fixlit(i64t, token, (uint64_t)1 << 63));
    sub = new_expr_ternary(token, cond, make_cast(i64t, token, sub, false), xorred, i64t);
  }
#endif

  return new_expr_cast(type, token, sub);
}

static bool cast_numbers(Expr **pLhs, Expr **pRhs, bool make_int) {
  Expr *lhs = *pLhs;
  Expr *rhs = *pRhs;
  Type *ltype = lhs->type;
  Type *rtype = rhs->type;
  assert(ltype != NULL);
  assert(rtype != NULL);
  if (!is_number(ltype)) {
    parse_error(PE_FATAL, lhs->token, "number type expected");
    return false;
  }
  if (!is_number(rtype)) {
    parse_error(PE_FATAL, rhs->token, "number type expected");
    return false;
  }

  {
    bool lflo = is_flonum(ltype), rflo = is_flonum(rtype);
    if (lflo || rflo) {
      int dir = !lflo ? 1 : !rflo ? -1 : (int)rtype->flonum.kind - (int)ltype->flonum.kind;
      if (dir < 0)
        *pRhs = make_cast(ltype, rhs->token, rhs, false);
      else if (dir > 0)
        *pLhs = make_cast(rtype, lhs->token, lhs, false);
      return true;
    }
  }
  enum FixnumKind lkind = ltype->fixnum.kind;
  enum FixnumKind rkind = rtype->fixnum.kind;
  bool changed = false;
  if (ltype->fixnum.kind >= FX_ENUM) {
    ltype = &tyInt;
    lkind = FX_INT;
    changed = true;
  }
  if (rtype->fixnum.kind >= FX_ENUM) {
    rtype = &tyInt;
    rkind = FX_INT;
    changed = true;
  }

  if (make_int && lkind < FX_INT && rkind < FX_INT) {
    *pLhs = promote_to_int(lhs);
    *pRhs = promote_to_int(rhs);
  } else if (changed || !same_type_without_qualifier(ltype, rtype, true)) {
    int l = (type_size(ltype) << 1) | (ltype->fixnum.is_unsigned ? 1 : 0);
    int r = (type_size(rtype) << 1) | (rtype->fixnum.is_unsigned ? 1 : 0);
    Type *type = l >= r ? ltype : rtype;
    *pLhs = make_cast(type, lhs->token, lhs, false);
    *pRhs = make_cast(type, rhs->token, rhs, false);
  }
  return true;
}

static Expr *reduce_refer_deref_add(Expr *expr, Type *subtype, Expr *lhs, Fixnum rhs) {
  if (lhs->kind == EX_FIXNUM) {
    return new_expr_unary(EX_DEREF, expr->type, expr->token,
                          new_expr_fixlit(subtype, lhs->token, lhs->fixnum + rhs));
  } else if (lhs->kind == EX_DEREF && lhs->unary.sub->kind == EX_FIXNUM) {
    // Concat 2 deref.
    Expr *sub = lhs->unary.sub;
    return new_expr_unary(EX_DEREF, expr->type, expr->token,
                          new_expr_fixlit(sub->type, sub->token, sub->fixnum + rhs));
  } else if (lhs->kind == EX_ADD && lhs->bop.rhs->kind == EX_FIXNUM) {
    // *(((lhs->lhs) + (lhs->rhs)) + rhs) => *(lhs->lhs + (lhs->rhs + rhs))
    return new_expr_unary(EX_DEREF, expr->type, expr->token,
                          new_expr_bop(EX_ADD, subtype, lhs->token, lhs->bop.lhs,
                                       new_expr_fixlit(lhs->bop.rhs->type, lhs->bop.rhs->token,
                                                       lhs->bop.rhs->fixnum + rhs)));
  }
  return NULL;
}

Expr *reduce_refer(Expr *expr) {
  // target->field => *(target + offset(field))
  switch (expr->kind) {
  case EX_MEMBER:
    {
      Expr *target = reduce_refer(expr->member.target);
      if (target->type->kind == TY_STRUCT && target->kind == EX_DEREF) {
        // (*sub).field => sub->field
        target = target->unary.sub;
      }

      const MemberInfo *minfo = expr->member.info;
      Type *ptype = minfo->type;
      ptype = ptrof(ptype->kind == TY_ARRAY ? array_to_ptr(ptype) : ptype);
      // target->field => *(target + offset(field))
      Expr *result = reduce_refer_deref_add(expr, ptype, target, minfo->offset);
      if (result != NULL)
        return result;

      // Transform member access to pointer dereference, only if target is referenceable.
      // target->field => *(target + offset(field))
      Type *type = target->type;
      switch (target->kind) {
      case EX_VAR:
#if STRUCT_ARG_AS_POINTER
        if (type->kind == TY_STRUCT) {
          VarInfo *varinfo = scope_find(target->var.scope, target->var.name, NULL);
          assert(varinfo != NULL);
          if (varinfo->storage & VS_PARAM && !is_small_struct(type)) {  // The parameter is passed by reference.
            if (varinfo->type->kind == TY_PTR) {
              assert(varinfo->type->pa.ptrof == type);
              target->type = type = varinfo->type;
            } else {
              // The type in scope is modified in `gen` phase,
              // so it might be remained as `struct`.
              assert(same_type(type, varinfo->type));
              target->type = type = ptrof(type);
            }
          }
        }
#endif
        // Fallthrough
      case EX_DEREF:
        if (type->kind == TY_STRUCT) {
          // target.field => (&target)->field
          target = new_expr_unary(EX_REF, ptrof(type), target->token, target);
        }
        return new_expr_unary(EX_DEREF, minfo->type, expr->token,
                              new_expr_bop(EX_ADD, ptrof(minfo->type), expr->token, target,
                                           new_expr_fixlit(&tySize, expr->token, minfo->offset)));
      default:
        // ex. funcall().x cannot be taken its reference, so keep the expression.
        break;
      }
    }
    break;
  case EX_DEREF:
    {
      Expr *sub = expr->unary.sub;
      if (sub->kind == EX_ADD) {
        // *(lhs + rhs) => *lhs + rhs
        Expr *lhs, *rhs;
        if (sub->bop.lhs->kind != EX_FIXNUM) {
          lhs = sub->bop.lhs;
          rhs = sub->bop.rhs;
        } else {
          lhs = sub->bop.rhs;
          rhs = sub->bop.lhs;
        }
        Expr *lhs2 = reduce_refer(lhs);
        if (rhs->kind == EX_FIXNUM) {
          Expr *result = reduce_refer_deref_add(expr, sub->type, lhs2, rhs->fixnum);
          if (result != NULL)
            return result;
        }
        if (lhs2 != lhs)
          return new_expr_unary(EX_DEREF, expr->type, expr->token,
                                new_expr_bop(EX_ADD, sub->type, sub->token, lhs2, rhs));
      }
    }
    break;
  default: break;
  }
  return expr;
}

static void check_referable(const Token *tok, Expr *expr, const char *error) {
  if (expr->kind == EX_COMPLIT)
    return;
  check_lval(tok, expr, error);
}

Expr *make_refer(const Token *tok, Expr *expr) {
  check_referable(tok, expr, "Cannot take reference");

  expr = reduce_refer(expr);

  if (expr->kind == EX_DEREF)
    return expr->unary.sub;
  Expr *e = expr;
  if (e->kind == EX_COMPLIT)
    e = e->complit.var;
  if (e->kind == EX_VAR) {
    VarInfo *varinfo = scope_find(e->var.scope, e->var.name, NULL);
    if (varinfo == NULL) {
      // Variable undeclared error must be raised already, so ignore here.
    } else {
      varinfo->storage |= VS_REF_TAKEN;
      if (varinfo->storage & VS_STATIC && !is_global_scope(e->var.scope)) {
        VarInfo *gvarinfo = varinfo->static_.svar;
        gvarinfo->storage |= VS_REF_TAKEN;
      }
    }
  }
  return new_expr_unary(EX_REF, ptrof(expr->type), tok, expr);
}

Expr *promote_to_int(Expr *expr) {
  assert(expr->type->kind == TY_FIXNUM);
  enum FixnumKind kind = expr->type->fixnum.kind;
  if (kind >= FX_INT && kind <= FX_LLONG)
    return expr;
  return make_cast(&tyInt, expr->token, expr, false);
}

Expr *new_expr_num_bop(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs) {
  if (kind == EX_LSHIFT || kind == EX_RSHIFT) {
    lhs = promote_to_int(lhs);
    rhs = make_cast(lhs->type, rhs->token, rhs, false);
  } else {
    cast_numbers(&lhs, &rhs, true);
  }

  do {
    if (is_const(rhs) && is_number(rhs->type)) {
      if (is_const(lhs) && is_number(lhs->type)) {
#ifndef __NO_FLONUM
        if (is_flonum(lhs->type)) {
          assert(is_flonum(rhs->type));
          Flonum lval = lhs->flonum;
          Flonum rval = rhs->flonum;
          Flonum value;
          switch (kind) {
          case EX_MUL:     value = lval * rval; break;
          case EX_DIV:     value = lval / rval; break;
          default:
            assert(!"err");
            value = -1;  // Dummy
            break;
          }
          Type *type = lhs->type;
          if (is_flonum(rhs->type))
            type = rhs->type;
          if (is_flonum(type)) {
            return new_expr_flolit(type, lhs->token, value);
          } else {
            Fixnum fixnum = value;
            return new_expr_fixlit(type, lhs->token, fixnum);
          }
        }
#endif

        if ((kind == EX_DIV || kind == EX_MOD) && rhs->fixnum == 0)
          break;

#define CALC(kind, lval, rval, value) \
  switch (kind) { \
  default: assert(false); /* Fallthrough */ \
  case EX_MUL:     value = lval * rval; break; \
  case EX_DIV:     value = lval / rval; break; \
  case EX_MOD:     value = lval % rval; break; \
  case EX_BITAND:  value = lval & rval; break; \
  case EX_BITOR:   value = lval | rval; break; \
  case EX_BITXOR:  value = lval ^ rval; break; \
  case EX_LSHIFT:  value = lval << rval; break; \
  case EX_RSHIFT:  value = lval >> rval; break; \
  }

        Fixnum value;
        if (lhs->type->fixnum.is_unsigned) {
          UFixnum lval = lhs->fixnum;
          UFixnum rval = rhs->fixnum;
          CALC(kind, lval, rval, value)
        } else {
          Fixnum lval = lhs->fixnum;
          Fixnum rval = rhs->fixnum;
          CALC(kind, lval, rval, value)
        }
#undef CALC
        enum FixnumKind lk = lhs->type->fixnum.kind, rk = rhs->type->fixnum.kind;
        Type *type = lk >= rk ? lhs->type : rhs->type;
        if (type->fixnum.kind < FX_INT)
          type = &tyInt;
        value = wrap_value(value, type_size(type), type->fixnum.is_unsigned);
        return new_expr_fixlit(type, lhs->token, value);
      } else {
#ifndef __NO_FLONUM
        if (is_flonum(rhs->type)) {
          assert(is_flonum(lhs->type));
          Flonum rval = rhs->flonum;
          switch (kind) {
          case EX_MUL:
            if (rval == 0.0)
              return new_expr_bop(EX_COMMA, rhs->type, tok, lhs, rhs);  // 0.0
            if (rval == -1.0)
              return new_expr_unary(EX_NEG, lhs->type, lhs->token, lhs);  // -lhs
            // Fallthrough.
          case EX_DIV:
            if (rval == 1.0)
              return lhs;  // no effect.
            break;
          default: break;
          }
          break;
        }
#endif
        Fixnum rval = rhs->fixnum;
        switch (kind) {
        case EX_MUL:
          if (rval == 0)
            return new_expr_bop(EX_COMMA, rhs->type, tok, lhs, rhs);  // 0
          if (rval == -1)
            return new_expr_unary(EX_NEG, lhs->type, lhs->token, lhs);
          // Fallthrough.
        case EX_DIV:
          if (rval == 1)
            return lhs;  // no effect.
          break;
        case EX_BITAND:
          if (rval == 0)
            return new_expr_bop(EX_COMMA, rhs->type, tok, lhs, rhs);  // 0
          break;
        case EX_BITOR:
        case EX_BITXOR:
        case EX_LSHIFT:
        case EX_RSHIFT:
          if (rval == 0)
            return lhs;  // no effect.
          break;
        default: break;
        }
      }
    } else {
      if (is_const(lhs) && is_number(lhs->type)) {
#ifndef __NO_FLONUM
        if (is_flonum(lhs->type)) {
          assert(is_flonum(rhs->type));
          Flonum lval = lhs->flonum;
          switch (kind) {
          case EX_MUL:
            if (lval == 0.0)
              return new_expr_bop(EX_COMMA, lhs->type, tok, rhs, lhs);  // 0.0
            if (lval == 1.0)
              return rhs;  // no effect.
            if (lval == -1.0)
              return new_expr_unary(EX_NEG, rhs->type, rhs->token, rhs);  // -rhs
            break;
          default: break;
          }
          break;
        }
#endif
        Fixnum lval = rhs->fixnum;
        switch (kind) {
        case EX_MUL:
          if (lval == 0)
            return new_expr_bop(EX_COMMA, lhs->type, tok, rhs, lhs);  // 0
          if (lval == -1)
            return new_expr_unary(EX_NEG, rhs->type, rhs->token, rhs);  // -rhs
          // Fallthrough.
        case EX_DIV:
          if (lval == 1)
            return rhs;  // no effect.
          break;
        case EX_BITAND:
          if (lval == 0)
            return new_expr_bop(EX_COMMA, lhs->type, tok, rhs, lhs);  // 0
          break;
        case EX_BITOR:
        case EX_BITXOR:
          if (lval == 0)
            return rhs;  // no effect.
          break;
        case EX_LSHIFT:
        case EX_RSHIFT:
          if (lval == 0)
            return lhs;  // no effect.
          break;
        default: break;
        }
      }
    }
  } while (0);

  if ((kind == EX_DIV || kind == EX_MOD) && is_const(rhs) &&
      is_fixnum(rhs->type->kind) && rhs->fixnum == 0) {
    parse_error(PE_WARNING, rhs->token, "Divide by 0");
  }

  return new_expr_bop(kind, lhs->type, tok, lhs, rhs);
}

Expr *new_expr_int_bop(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs) {
  if (!is_fixnum(lhs->type->kind))
    parse_error(PE_FATAL, lhs->token, "int type expected");
  if (!is_fixnum(rhs->type->kind))
    parse_error(PE_FATAL, rhs->token, "int type expected");
  return new_expr_num_bop(kind, tok, lhs, rhs);
}

Expr *new_expr_addsub(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs) {
  lhs = str_to_char_array_var(curscope, lhs);
  rhs = str_to_char_array_var(curscope, rhs);

  Type *type = NULL;
  Type *ltype = lhs->type;
  Type *rtype = rhs->type;
  assert(ltype != NULL);
  assert(rtype != NULL);
  if (is_number(ltype) && is_number(rtype)) {
    if (is_const(lhs) && is_const(rhs)) {
#ifndef __NO_FLONUM
      if (is_flonum(lhs->type) || is_flonum(rhs->type)) {
        Flonum lval = is_flonum(lhs->type) ? lhs->flonum : lhs->fixnum;
        Flonum rval = is_flonum(rhs->type) ? rhs->flonum : rhs->fixnum;
        Flonum value;
        switch (kind) {
        case EX_ADD:     value = lval + rval; break;
        case EX_SUB:     value = lval - rval; break;
        default:
          assert(!"err");
          value = -1;  // Dummy
          break;
        }
        Type *type = lhs->type;
        if (is_flonum(rhs->type))
          type = rhs->type;
        if (is_flonum(type)) {
          return new_expr_flolit(type, lhs->token, value);
        } else {
          Fixnum fixnum = value;
          return new_expr_fixlit(type, lhs->token, fixnum);
        }
      }
#endif
      enum FixnumKind lnt = ltype->fixnum.kind;
      enum FixnumKind rnt = rtype->fixnum.kind;
      if (lnt >= FX_ENUM) {
        ltype = &tyInt;
        lnt = FX_INT;
      }
      if (rnt >= FX_ENUM) {
        rtype = &tyInt;
        rnt = FX_INT;
      }

      Fixnum lval = lhs->fixnum;
      Fixnum rval = rhs->fixnum;
      Fixnum value;
      switch (kind) {
      case EX_ADD: value = lval + rval; break;
      case EX_SUB: value = lval - rval; break;
      default:
        assert(false);
        value = -1;
        break;
      }
      Type *type = lnt >= rnt ? ltype : rtype;
      if (type->fixnum.kind < FX_INT)
        type = &tyInt;
      return new_expr_fixlit(type, lhs->token,
                             wrap_value(value, type_size(type), type->fixnum.is_unsigned));
    }

    cast_numbers(&lhs, &rhs, true);
    type = lhs->type;
  } else if (ptr_or_array(ltype)) {
    if (is_fixnum(rtype->kind)) {
      type = ltype;
      if (ltype->kind == TY_ARRAY)
        type = array_to_ptr(ltype);
      // lhs + ((size_t)rhs * sizeof(*lhs))
      if (!ensure_struct(type->pa.ptrof, tok, curscope))
        return lhs;
      rhs = new_expr_num_bop(EX_MUL, rhs->token,
                             make_cast(&tySize, rhs->token, rhs, false),
                             calc_type_size(type->pa.ptrof));
    } else if (kind == EX_SUB && ptr_or_array(rtype)) {
      if (ltype->kind == TY_ARRAY)
        ltype = array_to_ptr(ltype);
      if (rtype->kind == TY_ARRAY)
        rtype = array_to_ptr(rtype);
      if (!same_type_without_qualifier(ltype, rtype, true))
        parse_error(PE_FATAL, tok, "Different pointer diff");
      if (is_void_ptr(ltype)) {
        // void* - void*
        parse_error(PE_WARNING, tok, "Pointer subtraction of void*");
        ltype = rtype = ptrof(&tyChar);
        lhs = new_expr_cast(ltype, lhs->token, lhs);
        rhs = new_expr_cast(rtype, rhs->token, rhs);
      }
      // ((size_t)lhs - (size_t)rhs) / sizeof(*lhs)
      if (!ensure_struct(ltype->pa.ptrof, tok, curscope))
        return lhs;
      if (is_const(lhs) && is_const(rhs)) {
        assert(lhs->kind == EX_FIXNUM);
        assert(rhs->kind == EX_FIXNUM);
        return new_expr_fixlit(&tySize, tok,
                               (lhs->fixnum - rhs->fixnum) / type_size(ltype->pa.ptrof));
      }
      return new_expr_bop(
          EX_DIV, &tySSize, tok,
          make_cast(&tySSize, tok, new_expr_bop(EX_SUB, &tySize, tok, lhs, rhs), false),
          new_expr_fixlit(&tySSize, tok, type_size(ltype->pa.ptrof)));
    }
  } else if (ptr_or_array(rtype)) {
    if (kind == EX_ADD && is_fixnum(ltype->kind)) {
      type = rhs->type;
      if (type->kind == TY_ARRAY)
        type = array_to_ptr(type);
      // ((size_t)lhs * sizeof(*rhs)) + rhs
      if (!ensure_struct(type->pa.ptrof, tok, curscope))
        return rhs;
      Expr *tmp = new_expr_num_bop(EX_MUL, lhs->token,
                                   make_cast(&tySize, lhs->token, lhs, false),
                                   new_expr_fixlit(&tySize, tok, type_size(type->pa.ptrof)));
      lhs = rhs;
      rhs = tmp;
      Type *t = ltype;
      ltype = rtype;
      rtype = t;
    }
  }
  if (type == NULL) {
    parse_error(PE_NOFATAL, tok, "Cannot apply `%.*s'", (int)(tok->end - tok->begin), tok->begin);
    type = ltype;  // Dummy
  } else if (ptr_or_array(ltype) && is_const(lhs) && is_const(rhs)) {
    assert(lhs->kind == EX_FIXNUM);
    if (kind == EX_ADD) {
      lhs->fixnum += rhs->fixnum;
    } else {
      assert(kind == EX_SUB);
      lhs->fixnum -= rhs->fixnum;
    }
    return lhs;
  }
  return new_expr_bop(kind, type, tok, lhs, rhs);
}

#ifndef __NO_BITFIELD
Expr *extract_bitfield_value(Expr *src, const MemberInfo *minfo) {
  Expr *tmp = src;
  Type *type = src->type;
  if (type->fixnum.is_unsigned) {
    tmp = src;
    if (minfo->bitfield.position > 0)
      tmp = new_expr_bop(EX_RSHIFT, tmp->type, tmp->token, tmp,
                         new_expr_fixlit(tmp->type, tmp->token, minfo->bitfield.position));
    UFixnum mask = ((UFixnum)1 << minfo->bitfield.width) - 1;
    tmp = new_expr_bop(EX_BITAND, tmp->type, tmp->token, tmp,
                       new_expr_fixlit(tmp->type, tmp->token, mask));
  } else {
    int w = MAX(type_size(type), MINREGSIZE) * TARGET_CHAR_BIT;
    int l = w - (minfo->bitfield.position + minfo->bitfield.width);
    tmp = src;
    if (l > 0)
      tmp = new_expr_bop(EX_LSHIFT, tmp->type, tmp->token, tmp,
                         new_expr_fixlit(tmp->type, tmp->token, l));
    if (minfo->bitfield.width < w)
      tmp = new_expr_bop(EX_RSHIFT, tmp->type, tmp->token, tmp,
                         new_expr_fixlit(tmp->type, tmp->token, w - minfo->bitfield.width));
  }
  return make_cast(minfo->type, src->token, tmp, false);
}

Expr *assign_bitfield_member(const Token *tok, Expr *dst, Expr *src, Expr *val,
                             const MemberInfo *minfo) {
  Type *type = dst->type;
  val = make_cast(type, val->token, val, false);

  UFixnum mask = ((UFixnum)1 << minfo->bitfield.width) - 1;
  Expr *val_masked = new_expr_num_bop(EX_BITAND, tok, val, new_expr_fixlit(type, tok, mask));
  val_masked = make_cast(type, tok, val_masked, false);
  if (minfo->bitfield.position > 0)
    val_masked = new_expr_num_bop(EX_LSHIFT, tok, val_masked,
                                  new_expr_fixlit(type, tok, minfo->bitfield.position));
  Expr *src_mask = new_expr_fixlit(type, tok, ~(mask << minfo->bitfield.position));
  Expr *src_masked = new_expr_num_bop(EX_BITAND, tok, src, src_mask);
  return new_expr_bop(EX_ASSIGN, type, tok, dst,
                      new_expr_bop(EX_BITOR, type, tok, val_masked, src_masked));
}

Expr *assign_to_bitfield(const Token *tok, Expr *lhs, Expr *rhs, const MemberInfo *minfo) {
  // Transform expression to (ptr = &lhs, val = rhs, *ptr = (*ptr & ~(mask << bitpos)) | ((val & mask) << bitpos), val)

  Type *type = get_fixnum_type(minfo->bitfield.base_kind, minfo->type->fixnum.is_unsigned, 0);

  Type *ptype = ptrof(type);
  assert(!is_global_scope(curscope));
  Expr *ptr = alloc_tmp_var(curscope, ptype);
  Expr *ptr_assign = new_expr_bop(EX_ASSIGN, ptype, tok, ptr, make_refer(lhs->token, lhs));

  Type *vtype = rhs->type;
  Expr *dst = new_expr_unary(EX_DEREF, type, tok, ptr);
  Expr *assign;
  if (rhs->kind == EX_FIXNUM || rhs->kind == EX_VAR) {
    assign = assign_bitfield_member(tok, dst, dst, rhs, minfo);
  } else {
    Expr *val = alloc_tmp_var(curscope, vtype);
    Expr *val_assign = new_expr_bop(EX_ASSIGN, vtype, tok, val, rhs);
    assign = new_expr_bop(EX_COMMA, type, tok, val_assign,
                          assign_bitfield_member(tok, dst, dst, val, minfo));
  }
  // Extract bitfield again for the result:
  // If the result is not used then the calculation is eliminated so no need to worry about it.
  Expr *result = extract_bitfield_value(dst, minfo);
  return new_expr_bop(EX_COMMA, result->type, tok,
                      new_expr_bop(EX_COMMA, assign->type, tok, ptr_assign, assign),
                      result);
}

static Expr *transform_incdec_of_bitfield(enum ExprKind kind, Expr *target, const Token *tok,
                                          const MemberInfo *minfo) {
  // ++target => (&ptr = &target, src = *ptr, val = ((src + (1 << bitpos)) >> bitpos), *ptr = (src & (mask << bitpos)) | ((val & mask) << bitpos), val)
  // target++ => (&ptr = &target, src = *ptr, val = src >> bitpos, *ptr = (src & (mask << bitpos)) | (((val + 1) & mask) << bitpos), val)

  Type *type = get_fixnum_type(minfo->bitfield.base_kind, target->type->fixnum.is_unsigned, 0);

  Type *ptype = ptrof(type);
  assert(!is_global_scope(curscope));
  Expr *ptr = alloc_tmp_var(curscope, ptype);
  Expr *ptr_assign = new_expr_bop(EX_ASSIGN, ptype, tok, ptr, make_refer(target->token, target));
  Expr *dst = new_expr_unary(EX_DEREF, type, tok, ptr);

  Expr *src = alloc_tmp_var(curscope, type);
  Expr *src_assign = new_expr_bop(EX_ASSIGN, type, tok, src, dst);

  Type *vtype = minfo->type;
  Expr *val = alloc_tmp_var(curscope, vtype);

  enum {
    INCDEC  = 1 << 0,
    PREPOST = 1 << 1,
  };
  int dec = (kind - EX_PREINC) & INCDEC;
  int post = (kind - EX_PREINC) & PREPOST;

  Expr *val_assign, *after;
  if (post) {
    Expr *before = extract_bitfield_value(src, minfo);
    val_assign = new_expr_bop(EX_ASSIGN, vtype, tok, val, make_cast(vtype, tok, before, false));
    after = new_expr_bop(!dec ? EX_ADD : EX_SUB, vtype, tok, before, new_expr_fixlit(vtype, NULL, 1));
  } else {
    Expr *tmp = extract_bitfield_value(
        new_expr_bop(!dec ? EX_ADD : EX_SUB, type, tok, src,
                     new_expr_fixlit(type, NULL, 1LL << minfo->bitfield.position)),
        minfo);
    val_assign = new_expr_bop(EX_ASSIGN, vtype, tok, val, make_cast(vtype, tok, tmp, false));
    after = val;
  }
  Expr *store = assign_bitfield_member(tok, dst, src, after, minfo);

  return new_expr_bop(
      EX_COMMA, vtype, tok,
      new_expr_bop(EX_COMMA, vtype, tok, ptr_assign,
                   new_expr_bop(EX_COMMA, vtype, tok, src_assign,
                                new_expr_bop(EX_COMMA, vtype, tok, val_assign, store))),
      val);
}
#endif

Expr *incdec_of(enum ExprKind kind, Expr *target, const Token *tok) {
  check_referable(tok, target, "lvalue expected");
#ifndef __NO_BITFIELD
  if (target->kind == EX_MEMBER) {
    const MemberInfo *minfo = target->member.info;
    if (minfo->bitfield.width > 0)
      return transform_incdec_of_bitfield(kind, target, tok, minfo);
  }
#endif
  return new_expr_unary(kind, target->type, tok, target);
}

Expr *new_expr_cmp(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs) {
  if (lhs->type->kind == TY_FUNC)
    lhs = make_refer(lhs->token, lhs);
  if (rhs->type->kind == TY_FUNC)
    rhs = make_refer(rhs->token, rhs);

  // Adjust type for comparison.
  {
    Type *lt = lhs->type, *rt = rhs->type;
    if (is_number(lt) && is_number(rt)) {
      if (is_fixnum(lt->kind) && is_fixnum(rt->kind)) {
        if (lt->fixnum.kind < FX_INT)
          lhs = promote_to_int(lhs);
        if (rt->fixnum.kind < FX_INT)
          rhs = promote_to_int(rhs);
      }
      if (!cast_numbers(&lhs, &rhs, false))
        parse_error(PE_FATAL, tok, "Cannot compare except numbers");
    }
  }

  if (is_const(lhs) && is_const(rhs)) {
#define JUDGE(kind, tf, l, r)               \
  switch (kind) {                           \
  default: assert(false); /* Fallthrough */ \
  case EX_EQ: tf = l == r; break;           \
  case EX_NE: tf = l != r; break;           \
  case EX_LT: tf = l < r; break;            \
  case EX_LE: tf = l <= r; break;           \
  case EX_GE: tf = l >= r; break;           \
  case EX_GT: tf = l > r; break;            \
  }
    int tf = -1;
    switch (lhs->kind) {
    default:
      assert(false);
      // Fallthrough to suppress warning.
    case EX_FIXNUM:
      switch (rhs->kind) {
      case EX_FIXNUM:
        if (lhs->type->fixnum.is_unsigned || rhs->type->fixnum.is_unsigned) {
          UFixnum l = lhs->fixnum, r = rhs->fixnum;
          JUDGE(kind, tf, l, r);
        } else {
          Fixnum l = lhs->fixnum, r = rhs->fixnum;
          JUDGE(kind, tf, l, r);
        }
        break;
#ifndef __NO_FLONUM
      case EX_FLONUM: assert(false); break;
#endif
      case EX_STR:
        if (is_zero(lhs)) {
          switch (kind) {
          case EX_EQ: tf = false; break;
          case EX_NE: tf = true; break;
          default: break;
          }
        }
        break;
      default: break;
      }
      break;
#ifndef __NO_FLONUM
    case EX_FLONUM:
      switch (rhs->kind) {
      case EX_STR: break;
      case EX_FIXNUM: assert(false); break;
      case EX_FLONUM:
        {
          Flonum l = lhs->flonum;
          Flonum r;
          if (rhs->kind == EX_FLONUM) {
            r = rhs->flonum;
          } else if (rhs->kind == EX_FIXNUM) {
            r = rhs->type->fixnum.is_unsigned ? (Flonum)(UFixnum)rhs->fixnum : (Flonum)rhs->fixnum;
          } else {
            break;
          }
          JUDGE(kind, tf, l, r);
        }
        break;
      default: break;
      }
      break;
#endif
    case EX_STR:
      if (is_zero(rhs)) {
        switch (kind) {
        case EX_EQ: tf = false; break;
        case EX_NE: tf = true; break;
        default: break;
        }
      }
      break;
    }
    if (tf >= 0)
      return new_expr_fixlit(&tyBool, tok, tf);
#undef JUDGE
  }

  if ((kind == EX_EQ || kind == EX_NE) && (is_const(rhs) || is_const(lhs))) {
    Expr *v, *c;
    if (is_const(lhs)) {
      v = rhs;
      c = lhs;
    } else {
      v = lhs;
      c = rhs;
    }
    enum { NEVER = -1, UNKNOWN = -2 };
    int value = UNKNOWN;
    switch (c->kind) {
#ifndef __NO_FLONUM
    case EX_FLONUM:
      if (c->flonum == 0.0)
        value = 0;
      else if (c->flonum == 1.0)
        value = 1;
      else
        value = NEVER;
      break;
#endif
    case EX_FIXNUM:
      value = c->fixnum == 0 || c->fixnum == 1 ? c->fixnum : NEVER;
      break;
    case EX_STR:
      value = NEVER;
      break;
    default:
      break;
    }

    Expr *p = strip_cast(v);
    int k = kind;
    switch (value) {
    case 1:
      // Swap condition that regard the value as 0.
      k = (EX_EQ + EX_NE) - k;  // EQ <-> NE
      // Fallthrough
    case 0:
      // Eliminate comparing comparison result with 0.
      switch (p->kind) {
      case EX_EQ: case EX_NE:
        if (k == EX_EQ)
          p->kind = (EX_EQ + EX_NE) - p->kind;  // EQ <-> NE
        return p;
      case EX_LT: case EX_LE: case EX_GE: case EX_GT:
        if (k == EX_EQ)
          p->kind = EX_LT + ((p->kind - EX_LT) ^ 2);  // LT <-> GE, LE <-> GT
        return p;
      case EX_LOGAND: case EX_LOGIOR:
        if (k == EX_EQ)
          p = new_expr_bop(
              (EX_LOGAND + EX_LOGIOR) - p->kind,  // LOGAND <-> LOGIOR
              &tyBool, p->token,
              make_not_expr(p->bop.lhs->token, p->bop.lhs),
              make_not_expr(p->bop.rhs->token, p->bop.rhs));
        return p;
      default: break;
      }
      break;
    case NEVER:
      switch (p->kind) {
      case EX_EQ: case EX_NE:
      case EX_LT: case EX_LE: case EX_GE: case EX_GT:
      case EX_LOGAND: case EX_LOGIOR:
        parse_error(PE_WARNING, tok, "Always %s", kind != EX_EQ ? "true" : "false");
        return new_expr_bop(EX_COMMA, &tyBool, tok, v,
                            new_expr_fixlit(&tyBool, tok, (kind != EX_EQ)));
      default: break;
      }
      break;
    case UNKNOWN: default:
      break;
    }
  }

  lhs = str_to_char_array_var(curscope, lhs);
  rhs = str_to_char_array_var(curscope, rhs);

  Type *lt = lhs->type, *rt = rhs->type;
  if (ptr_or_array(lt) || ptr_or_array(rt)) {
    if (lt->kind == TY_ARRAY) {
      lt = array_to_ptr(lt);
      lhs = make_cast(lt, lhs->token, lhs, false);
    }
    if (rt->kind == TY_ARRAY) {
      rt = array_to_ptr(rt);
      rhs = make_cast(rt, rhs->token, rhs, false);
    }

    bool st;
    if (!((st = same_type_without_qualifier(lt, rt, true)) ||
          (lt->kind == TY_PTR && lt->pa.ptrof->kind == TY_VOID) ||
          (rt->kind == TY_PTR && rt->pa.ptrof->kind == TY_VOID) ||
          is_zero(rhs) || is_zero(lhs))) {
      enum ParseErrorLevel err = st || (lt->kind == TY_PTR && rt->kind == TY_PTR) ? PE_WARNING : PE_NOFATAL;
      parse_error(err, tok, "Compare pointer to other types");
    }
    if (rt->kind != TY_PTR)
      rhs = make_cast(lhs->type, rhs->token, rhs, false);
    else if (lt->kind != TY_PTR)
      lhs = make_cast(rhs->type, lhs->token, lhs, false);
  }

  return new_expr_bop(kind, &tyBool, tok, lhs, rhs);
}

Expr *make_cond(Expr *expr) {
  switch (expr->kind) {
  case EX_FIXNUM:
    expr = new_expr_fixlit(&tyBool, expr->token, expr->fixnum != 0);
    break;
#ifndef __NO_FLONUM
  case EX_FLONUM:
    expr = new_expr_fixlit(&tyBool, expr->token, expr->flonum != 0);
    break;
#endif
  case EX_STR:
    expr = new_expr_fixlit(&tyBool, expr->token, true);
    break;
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_LE:
  case EX_GE:
  case EX_GT:
  case EX_LOGAND:
  case EX_LOGIOR:
    break;
  case EX_COMMA:
    expr->bop.rhs = make_cond(expr->bop.rhs);
    expr->type = expr->bop.rhs->type;
    break;
  default:
    switch (expr->type->kind) {
    case TY_ARRAY:
    case TY_FUNC:
      expr = new_expr_fixlit(&tyBool, expr->token, true);
      break;
    default:
      expr = new_expr_cmp(
          EX_NE, expr->token, expr,
          make_cast(expr->type, expr->token, new_expr_fixlit(&tyInt, expr->token, 0), false));
      break;
    }
    break;
  }
  return expr;
}

Expr *make_not_expr(const Token *tok, Expr *expr) {
  Type *type = expr->type;
  assert(is_number(type) || ptr_or_array(type));
  if (type->kind == TY_ARRAY)
    type = array_to_ptr(type);
  Expr *zero;
#ifndef __NO_FLONUM
  if (is_flonum(type)) {
    zero = new_expr_flolit(type, tok, 0.0);
  } else
#endif
  {
    zero = new_expr_fixlit(type, tok, 0);
  }
  return new_expr_cmp(EX_EQ, tok, expr, zero);
}

static Expr *calc_assign_with(const Token *tok, Expr *lhs, Expr *rhs) {
  // Assume token-kind and expr-kind is same arrangement.
  enum ExprKind kind = tok->kind + (EX_ADD - TK_ADD_ASSIGN);
  switch (kind) {
  default:  assert(false);
    // Fallthrough to avoid compile error.
  case EX_ADD: case EX_SUB:
    return new_expr_addsub(kind, tok, lhs, rhs);
  case EX_MUL: case EX_DIV:
    return new_expr_num_bop(kind, tok, lhs, rhs);
  case EX_MOD: case EX_BITAND: case EX_BITOR: case EX_BITXOR:
    return new_expr_int_bop(kind, tok, lhs, rhs);
  case EX_LSHIFT: case EX_RSHIFT:
    {
      Type *ltype = lhs->type;
      Type *rtype = rhs->type;
      if (!is_fixnum(ltype->kind) || !is_fixnum(rtype->kind))
        parse_error(PE_FATAL, tok, "Cannot use `%.*s' except numbers.",
                    (int)(tok->end - tok->begin), tok->begin);
      return new_expr_bop(kind, ltype, tok, lhs, rhs);
    }
  }
}

#ifndef __NO_BITFIELD
static Expr *transform_assign_with_bitfield(const Token *tok, Expr *lhs, Expr *rhs,
                                            const MemberInfo *minfo) {
  // Transform expression to
  // (ptr = &lhs, src = *ptr, tmp = ((src >> bitpos) & mask) + rhs,
  //  *ptr = (src & ~(mask << bitpos)) | ((tmp & mask) << bitpos), (*ptr >> bitpos) & mask)

  Type *type = get_fixnum_type(minfo->bitfield.base_kind, lhs->type->fixnum.is_unsigned, 0);

  Type *ptype = ptrof(type);
  assert(!is_global_scope(curscope));
  Expr *ptr = alloc_tmp_var(curscope, ptype);
  Expr *ptr_assign = new_expr_bop(EX_ASSIGN, ptype, tok, ptr, make_refer(lhs->token, lhs));
  Expr *dst = new_expr_unary(EX_DEREF, type, tok, ptr);

  Expr *src = alloc_tmp_var(curscope, type);
  Expr *src_assign = new_expr_bop(EX_ASSIGN, type, tok, src, dst);

  Expr *tmp = extract_bitfield_value(src, minfo);
  tmp = calc_assign_with(tok, tmp, rhs);
  Expr *store = assign_bitfield_member(tok, dst, src, tmp, minfo);

  Type *vtype = rhs->type;
  Expr *result = extract_bitfield_value(dst, minfo);
  return new_expr_bop(EX_COMMA, result->type, tok,
                      new_expr_bop(EX_COMMA, vtype, tok, ptr_assign,
                                   new_expr_bop(EX_COMMA, vtype, tok, src_assign, store)),
                      result);
}
#endif

Expr *transform_assign_with(const Token *tok, Expr *lhs, Expr *rhs) {
  // Transform expression `lhs += rhs` to `lhs = lhs + rhs`.
  // If LHS is not a variable, add temporary variable to keep `&LHS` to avoid side effect.
  // Replace expression to (ptr = &lhs, *ptr = *ptr + rhs)
  Expr *tmp_assign = NULL;
  if (lhs->kind != EX_VAR) {
#ifndef __NO_BITFIELD
    if (lhs->kind == EX_MEMBER) {
      const MemberInfo *minfo = lhs->member.info;
      if (minfo->bitfield.width > 0)
        return transform_assign_with_bitfield(tok, lhs, rhs, minfo);
    }
#endif

    Type *ptype = ptrof(lhs->type);
    assert(!is_global_scope(curscope));
    Expr *ptr = alloc_tmp_var(curscope, ptype);
    tmp_assign = new_expr_bop(EX_ASSIGN, ptype, tok, ptr, make_refer(lhs->token, lhs));
    lhs = new_expr_unary(EX_DEREF, lhs->type, lhs->token, ptr);
  }

  Expr *bop = calc_assign_with(tok, lhs, rhs);
  Expr *result = new_expr_bop(EX_ASSIGN, lhs->type, tok, lhs,
                              make_cast(lhs->type, tok, bop, false));

  return tmp_assign == NULL ? result
                            : new_expr_bop(EX_COMMA, result->type, tok, tmp_assign, result);
}

static Expr *unnest_arg(Expr *arg, Vector *unnested) {
  Type *type = arg->type;
  assert(type->kind != TY_VOID);
  Expr *tmp = alloc_tmp_var(curscope, type);
  Expr *assign = new_expr_bop(EX_ASSIGN, type, arg->token, tmp, arg);
  assert(assign->kind == EX_ASSIGN && assign->bop.lhs->kind == EX_VAR);
  vec_push(unnested, assign);
  return tmp;
}

// If an argument is complex expression,
// precalculate it and make function argument simple.
static Expr *simplify_funarg_recur(Expr *arg, Vector *unnested) {
  switch (arg->kind) {
  case EX_TERNARY:
  case EX_FUNCALL:
  case EX_INLINED:
  case EX_BLOCK:
  case EX_LOGAND:  // Shortcut must be handled properly.
  case EX_LOGIOR:
    return unnest_arg(arg, unnested);

  case EX_COMMA:
    vec_push(unnested, arg->bop.lhs);
    return simplify_funarg_recur(arg->bop.rhs, unnested);

  case EX_COMPLIT:
    vec_push(unnested, arg);
    return arg->complit.var;

  // Binary operators
  case EX_MUL:
  case EX_DIV:
  case EX_MOD:
  case EX_LSHIFT:
  case EX_RSHIFT:
#if XCC_TARGET_ARCH == XCC_ARCH_X64
    // On x64, MUL, DIV and MOD instruction implicitly uses (breaks) %rdx
    // and %rdx is used as 3rd argument.
    // Similary, Shift instructions (SHL, SHR) uses %cl which is 4th argument.
    // so must be precalculated.
    return unnest_arg(arg, unnested);
#else
    // Except x64, these opcodes can be used in function argument.
    // Fallthrough
#endif
  case EX_ADD:
  case EX_SUB:
  case EX_BITAND:
  case EX_BITOR:
  case EX_BITXOR:
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_LE:
  case EX_GE:
  case EX_GT:
    arg->bop.lhs = simplify_funarg_recur(arg->bop.lhs, unnested);
    arg->bop.rhs = simplify_funarg_recur(arg->bop.rhs, unnested);
    break;

  case EX_ASSIGN:
    if (!is_prim_type(arg->type))
      return unnest_arg(arg, unnested);
    arg->bop.lhs = simplify_funarg_recur(arg->bop.lhs, unnested);
    arg->bop.rhs = simplify_funarg_recur(arg->bop.rhs, unnested);
    break;

  // Unary operators
  case EX_POS:
  case EX_NEG:
  case EX_BITNOT:
  case EX_PREINC:
  case EX_PREDEC:
  case EX_POSTINC:
  case EX_POSTDEC:
  case EX_REF:
  case EX_DEREF:
  case EX_CAST:
    arg->unary.sub = simplify_funarg_recur(arg->unary.sub, unnested);
    break;

  case EX_MEMBER:
    arg->member.target = simplify_funarg_recur(arg->member.target, unnested);
    break;

  // Literals
  case EX_FIXNUM:
  case EX_FLONUM:
  case EX_STR:
  case EX_VAR:
    break;
  }
  return arg;
}

Expr *simplify_funcall(Expr *funcall) {
  assert(funcall->kind == EX_FUNCALL);

  Vector *args = funcall->funcall.args;
  int arg_count = args->len;
  // To avoid nested funcall,
  // simplify funargs and precalculate complex expression before funcall.
  Vector *unnested = new_vector();
  for (int i = 0; i < arg_count; ++i) {
    Expr *arg = args->data[i];
    args->data[i] = simplify_funarg_recur(arg, unnested);
  }
  funcall->funcall.func = simplify_funarg_recur(funcall->funcall.func, unnested);

  if (unnested->len > 0) {
    const Token *token = funcall->token;
    Expr *comma = unnested->data[0];
    for (int i = 1; i < unnested->len; ++i) {
      Expr *rhs = unnested->data[i];
      comma = new_expr_bop(EX_COMMA, rhs->type, token, comma, rhs);
    }
    funcall = new_expr_bop(EX_COMMA, funcall->type, token, comma, funcall);
  }
  free_vector(unnested);
  return funcall;
}
