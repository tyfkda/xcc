#include "parser.h"

#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "ast.h"
#include "lexer.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

Scope *curscope;
Vector *toplevel;

static Table builtin_expr_ident_table;

static StructInfo *parse_struct(bool is_union);
static Expr *parse_cast_expr(void);
static Expr *parse_unary(void);

void add_builtin_expr_ident(const char *str, BuiltinExprProc *proc) {
  const Name *name = alloc_name(str, NULL, false);
  table_put(&builtin_expr_ident_table, name, proc);
}

static void define_enum_member(const Type *type, const Token *ident, int value) {
  VarInfo *varinfo = scope_add(curscope, ident, type, VS_ENUM_MEMBER);
  varinfo->enum_member.value = value;
}

void not_void(const Type *type, const Token *token) {
  if (type->kind == TY_VOID)
    parse_error(token, "`void' not allowed");
}

void not_const(const Type *type, const Token *token) {
  if (type->qualifier & TQ_CONST)
    parse_error(token, "Cannot modify `const'");
}

// Returns created global variable info.
VarInfo *str_to_char_array(Scope *scope, const Type *type, Initializer *init, Vector *toplevel) {
  assert(type->kind == TY_ARRAY && is_char_type(type->pa.ptrof));
  const Token *ident = alloc_ident(alloc_label(), NULL, NULL);
  VarInfo *varinfo = scope_add(scope, ident, type, VS_STATIC);
  if (is_global_scope(scope)) {
    Vector *decls = new_vector();
    vec_push(decls, new_vardecl(varinfo->type, ident, init, varinfo->storage));
    vec_push(toplevel, new_decl_vardecl(decls));
    varinfo->global.init = init;
  } else {
    varinfo->static_.gvar->global.init = init;
  }
  return varinfo;
}

Expr *str_to_char_array_var(Scope *scope, Expr *str, Vector *toplevel) {
  if (str->kind != EX_STR)
    return str;
  const Type* type = str->type;
  Initializer *init = malloc(sizeof(*init));
  init->kind = IK_SINGLE;
  init->single = str;
  init->token = str->token;

  VarInfo *varinfo = str_to_char_array(scope, type, init, toplevel);
  return new_expr_variable(varinfo->name, type, str->token, scope);
}

bool check_cast(const Type *dst, const Type *src, bool zero, bool is_explicit, const Token *token) {
  bool ok = can_cast(dst, src, zero, is_explicit);
  if (!ok || dst->kind == TY_ARRAY) {
    if (token == NULL)
      token = fetch_token();
    fprintf(stderr, "%s(%d): ", token->line->filename, token->line->lineno);

    fprintf(stderr, "Cannot convert value from type `");
    print_type(stderr, src);
    fprintf(stderr, "' to %s`", dst->kind == TY_ARRAY ? "array type " : "");
    print_type(stderr, dst);
    fprintf(stderr, "'\n");
    parse_error(token, NULL);
    return false;
  }
  return true;
}

Expr *make_cast(const Type *type, const Token *token, Expr *sub, bool is_explicit) {
  if (type->kind == TY_VOID || sub->type->kind == TY_VOID)
    parse_error(NULL, "cannot use `void' as a value");

  if (same_type(type, sub->type))
    return sub;
  if (is_const(sub) && is_number(sub->type) && is_number(type)) {
#ifndef __NO_FLONUM
    switch (sub->type->kind) {
    case TY_FLONUM:
      if (type->kind == TY_FIXNUM) {
        Fixnum fixnum = sub->flonum;
        return new_expr_fixlit(type, sub->token, fixnum);
      }
      sub->type = type;
      return sub;
    case TY_FIXNUM:
      if (type->kind == TY_FLONUM) {
        double flonum = sub->fixnum;
        return new_expr_flolit(type, sub->token, flonum);
      }
      break;
    default:
      break;
    }
#endif

    {
      int bytes = type_size(type);
      int src_bytes = type_size(sub->type);
      if (bytes < (int)type_size(&tySize) &&
          (bytes < src_bytes ||
           (bytes == src_bytes &&
            type->fixnum.is_unsigned != sub->type->fixnum.is_unsigned))) {
        int bits = bytes * CHAR_BIT;
        UFixnum mask = (-1UL) << bits;
        Fixnum value = sub->fixnum;
        if (!type->fixnum.is_unsigned &&  // signed
            (value & (1UL << (bits - 1))))  // negative
          value |= mask;
        else
          value &= ~mask;
        sub->fixnum = value;
      }
    }
    sub->type = type;
    return sub;
  }

  check_cast(type, sub->type, is_zero(sub), is_explicit, token);
  if (sub->kind == EX_CAST) {
    sub->type = type;
    return sub;
  }

  return new_expr_cast(type, token, sub);
}

static bool cast_numbers(Expr **pLhs, Expr **pRhs, bool keep_left) {
  Expr *lhs = *pLhs;
  Expr *rhs = *pRhs;
  const Type *ltype = lhs->type;
  const Type *rtype = rhs->type;
  assert(ltype != NULL);
  assert(rtype != NULL);
  if (!is_number(ltype)) {
    parse_error(lhs->token, "number type expected");
    return false;
  }
  if (!is_number(rtype)) {
    parse_error(rhs->token, "number type expected");
    return false;
  }

#ifndef __NO_FLONUM
  {
    bool lflo = is_flonum(ltype), rflo = is_flonum(rtype);
    if (lflo || rflo) {
      int dir = !lflo ? 1 : !rflo ? -1 : (int)rtype->flonum.kind - (int)ltype->flonum.kind;
      if (dir < 0 || keep_left)
        *pRhs = make_cast(ltype, rhs->token, rhs, false);
      else if (dir > 0)
        *pLhs = make_cast(rtype, lhs->token, lhs, false);
      return true;
    }
  }
#endif
  enum FixnumKind lkind = ltype->fixnum.kind;
  enum FixnumKind rkind = rtype->fixnum.kind;
  if (ltype->fixnum.kind == FX_ENUM) {
    ltype = &tyInt;
    lkind = FX_INT;
  }
  if (rtype->fixnum.kind == FX_ENUM) {
    rtype = &tyInt;
    rkind = FX_INT;
  }

  int l = (lkind << 1) | (ltype->fixnum.is_unsigned ? 1 : 0);
  int r = (rkind << 1) | (rtype->fixnum.is_unsigned ? 1 : 0);
  if (keep_left || l > r)
    *pRhs = make_cast(ltype, rhs->token, rhs, false);
  else if (l < r)
    *pLhs = make_cast(rtype, lhs->token, lhs, false);
  return true;
}

static void check_lval(const Token *tok, Expr *expr, const char *error) {
  switch (expr->kind) {
  case EX_VAR:
  case EX_DEREF:
  case EX_MEMBER:
    break;
  default:
    parse_error(tok, error);
    break;
  }
}

static void check_referable(const Token *tok, Expr *expr, const char *error) {
  if (expr->kind == EX_COMPLIT)
    return;
  check_lval(tok, expr, error);
}

Expr *make_refer(const Token *tok, Expr *expr) {
  check_referable(tok, expr, "Cannot take reference");
  if (expr->kind == EX_DEREF)
    return expr->unary.sub;
  if (expr->kind == EX_VAR) {
    VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, NULL);
    assert(varinfo != NULL);
    varinfo->storage |= VS_REF_TAKEN;
  }
  return new_expr_unary(EX_REF, ptrof(expr->type), tok, expr);
}

static Expr *new_expr_num_bop(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  if (is_const(lhs) && is_number(lhs->type) &&
      is_const(rhs) && is_number(rhs->type)) {
#ifndef __NO_FLONUM
    if (is_flonum(lhs->type) || is_flonum(rhs->type)) {
      double lval = is_flonum(lhs->type) ? lhs->flonum : lhs->fixnum;
      double rval = is_flonum(rhs->type) ? rhs->flonum : rhs->fixnum;
      double value;
      switch (kind) {
      case EX_MUL:     value = lval * rval; break;
      case EX_DIV:     value = lval / rval; break;
      default:
        assert(!"err");
        value = -1;  // Dummy
        break;
      }
      const Type *type = lhs->type;
      if (!keep_left && is_flonum(rhs->type))
        type = rhs->type;
      if (is_flonum(type)) {
        return new_expr_flolit(type, lhs->token, value);
      } else {
        Fixnum fixnum = value;
        return new_expr_fixlit(type, lhs->token, fixnum);
      }
    }
#endif

#define CALC(kind, l, r, value) \
  switch (kind) { \
  default: assert(false); /* Fallthrough */ \
  case EX_MUL:     value = l * r; break; \
  case EX_DIV:     value = l / r; break; \
  case EX_MOD:     value = l % r; break; \
  case EX_BITAND:  value = l & r; break; \
  case EX_BITOR:   value = l | r; break; \
  case EX_BITXOR:  value = l ^ r; break; \
  }

    Fixnum value;
    if (lhs->type->fixnum.is_unsigned) {
      UFixnum l = lhs->fixnum;
      UFixnum r = rhs->fixnum;
      CALC(kind, l, r, value)
    } else {
      Fixnum l = lhs->fixnum;
      Fixnum r = rhs->fixnum;
      CALC(kind, l, r, value)
    }
#undef CALC
    const Type *type = keep_left || lhs->type->fixnum.kind >= rhs->type->fixnum.kind ? lhs->type : rhs->type;
    return new_expr_fixlit(type, lhs->token, value);
  }

  cast_numbers(&lhs, &rhs, keep_left);
  return new_expr_bop(kind, lhs->type, tok, lhs, rhs);
}

static Expr *new_expr_int_bop(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  if (!is_fixnum(lhs->type->kind))
    parse_error(lhs->token, "int type expected");
  if (!is_fixnum(rhs->type->kind))
    parse_error(rhs->token, "int type expected");
  return new_expr_num_bop(kind, tok, lhs, rhs, keep_left);
}

Expr *new_expr_addsub(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  const Type *type = NULL;
  const Type *ltype = lhs->type;
  const Type *rtype = rhs->type;
  assert(ltype != NULL);
  assert(rtype != NULL);
  if (is_number(ltype) && is_number(rtype)) {
    if (is_const(lhs) && is_const(rhs)) {
#ifndef __NO_FLONUM
      if (is_flonum(lhs->type) || is_flonum(rhs->type)) {
        double lval = is_flonum(lhs->type) ? lhs->flonum : lhs->fixnum;
        double rval = is_flonum(rhs->type) ? rhs->flonum : rhs->fixnum;
        double value;
        switch (kind) {
        case EX_ADD:     value = lval + rval; break;
        case EX_SUB:     value = lval - rval; break;
        default:
          assert(!"err");
          value = -1;  // Dummy
          break;
        }
        const Type *type = lhs->type;
        if (!keep_left && is_flonum(rhs->type))
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
      if (lnt == FX_ENUM)
        lnt = FX_INT;
      if (rnt == FX_ENUM)
        rnt = FX_INT;

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
      const Type *type = lnt >= rnt ? lhs->type : rhs->type;
      return new_expr_fixlit(type, lhs->token, value);
    }

    cast_numbers(&lhs, &rhs, keep_left);
    type = lhs->type;
  } else if (ptr_or_array(ltype)) {
    if (is_fixnum(rtype->kind)) {
      type = ltype;
      if (ltype->kind == TY_ARRAY)
        type = array_to_ptr(ltype);
      // lhs + ((size_t)rhs * sizeof(*lhs))
      rhs = new_expr_num_bop(EX_MUL, rhs->token,
                             make_cast(&tySize, rhs->token, rhs, false),
                             new_expr_fixlit(&tySize, tok, type_size(type->pa.ptrof)), false);
    } else if (kind == EX_SUB && ptr_or_array(rtype)) {
      if (ltype->kind == TY_ARRAY)
        ltype = array_to_ptr(ltype);
      if (rtype->kind == TY_ARRAY)
        rtype = array_to_ptr(rtype);
      if (!same_type(ltype, rtype))
        parse_error(tok, "Different pointer diff");
      // ((size_t)lhs - (size_t)rhs) / sizeof(*lhs)
      return new_expr_bop(EX_DIV, &tySSize, tok,
                          new_expr_bop(EX_SUB, &tySSize, tok, lhs, rhs),
                          new_expr_fixlit(&tySSize, tok, type_size(ltype->pa.ptrof)));
    }
  } else if (ptr_or_array(rtype)) {
    if (kind == EX_ADD && is_fixnum(ltype->kind) && !keep_left) {
      type = rhs->type;
      if (type->kind == TY_ARRAY)
        type = array_to_ptr(type);
      // ((size_t)lhs * sizeof(*rhs)) + rhs
      lhs = new_expr_num_bop(EX_MUL, lhs->token,
                             make_cast(&tySize, lhs->token, lhs, false),
                             new_expr_fixlit(&tySize, tok, type_size(type->pa.ptrof)), false);
    }
  }
  if (type == NULL) {
    parse_error(tok, "Cannot apply `%.*s'", (int)(tok->end - tok->begin), tok->begin);
  }
  return new_expr_bop(kind, type, tok, lhs, rhs);
}

static Expr *new_expr_incdec(enum ExprKind kind, const Token *tok, Expr *sub) {
  check_referable(tok, sub, "lvalue expected");
  return new_expr_unary(kind, sub->type, tok, sub);
}

static enum ExprKind swap_cmp(enum ExprKind kind) {
  assert(EX_EQ <= kind && kind <= EX_GT);
  if (kind >= EX_LT)
    kind = EX_GT - (kind - EX_LT);
  return kind;
}

static Expr *new_expr_cmp(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs) {
  const Type *lt = lhs->type, *rt = rhs->type;
  if (ptr_or_array(lt) || ptr_or_array(rt)) {
    if (lt->kind == TY_ARRAY) {
      lt = array_to_ptr(lt);
      lhs = make_cast(lt, lhs->token, lhs, false);
    }
    if (rt->kind == TY_ARRAY) {
      rt = array_to_ptr(rt);
      rhs = make_cast(rt, rhs->token, rhs, false);
    }
    if (lt->kind != TY_PTR) {  // For comparison between pointer and 0.
      Expr *tmp = lhs;
      lhs = rhs;
      rhs = tmp;
      const Type *tt = lt;
      lt = rt;
      rt = tt;
      kind = swap_cmp(kind);
    }
    if (!can_cast(lt, rt, is_zero(rhs), false))
      parse_error(tok, "Cannot compare pointer to other types");
    if (rt->kind != TY_PTR)
      rhs = make_cast(lhs->type, rhs->token, rhs, false);
  } else {
    if (!cast_numbers(&lhs, &rhs, false))
      parse_error(tok, "Cannot compare except numbers");

    if (is_const(lhs) && is_const(rhs)) {
#define JUDGE(kind, tf, l, r)  \
switch (kind) { \
default: assert(false); /* Fallthrough */ \
case EX_EQ:  tf = l == r; break; \
case EX_NE:  tf = l != r; break; \
case EX_LT:  tf = l < r; break; \
case EX_LE:  tf = l <= r; break; \
case EX_GE:  tf = l >= r; break; \
case EX_GT:  tf = l > r; break; \
}
      bool tf;
      switch (lhs->kind) {
      default:
        assert(false);
        // Fallthrough to suppress warning.
      case EX_FIXNUM:
        assert(rhs->kind == EX_FIXNUM);
        if (lhs->type->fixnum.is_unsigned) {
          UFixnum l = lhs->fixnum, r = rhs->fixnum;
          JUDGE(kind, tf, l, r);
        } else {
          Fixnum l = lhs->fixnum, r = rhs->fixnum;
          JUDGE(kind, tf, l, r);
        }
        break;
#ifndef __NO_FLONUM
      case EX_FLONUM:
        {
          assert(rhs->kind == EX_FLONUM);
          double l = lhs->flonum, r = rhs->flonum;
          JUDGE(kind, tf, l, r);
        }
        break;
#endif
      }
      return new_expr_fixlit(&tyBool, tok, tf);
#undef JUDGE
    }
  }
  return new_expr_bop(kind, &tyBool, tok, lhs, rhs);
}

//

Expr *make_cond(Expr *expr) {
  switch (expr->kind) {
  case EX_FIXNUM:
    break;
#ifndef __NO_FLONUM
  case EX_FLONUM:
    expr = new_expr_fixlit(&tyBool, expr->token, expr->flonum != 0);
    break;
#endif
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_LE:
  case EX_GE:
  case EX_GT:
  case EX_LOGAND:
  case EX_LOGIOR:
    break;
  default:
    switch (expr->type->kind) {
    case TY_ARRAY:
    case TY_FUNC:
      expr = new_expr_fixlit(&tyBool, expr->token, true);
      break;
    default:
      {
        Expr *zero = make_cast(expr->type, expr->token, new_expr_fixlit(&tyInt, expr->token, 0), false);
        expr = new_expr_cmp(EX_NE, expr->token, expr, zero);
      }
      break;
    }
    break;
  }
  return expr;
}

static Expr *make_not_cond(Expr *expr) {
  Expr *cond = make_cond(expr);
  enum ExprKind kind = cond->kind;
  switch (kind) {
  case EX_FIXNUM:
    cond = new_expr_fixlit(&tyBool, expr->token, cond->fixnum == 0);
    break;
#ifndef __NO_FLONUM
  case EX_FLONUM:
    expr = new_expr_fixlit(&tyBool, expr->token, expr->flonum == 0);
    break;
#endif
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_LE:
  case EX_GE:
  case EX_GT:
    if (kind <= EX_NE)
      kind = (EX_EQ + EX_NE) - kind;
    else
      kind = EX_LT + ((kind - EX_LT) ^ 2);
    cond->kind = kind;
    break;
  case EX_LOGAND:
  case EX_LOGIOR:
    {
      Expr *lhs = make_not_cond(cond->bop.lhs);
      Expr *rhs = make_not_cond(cond->bop.rhs);
      cond = new_expr_bop((EX_LOGAND + EX_LOGIOR) - kind, &tyBool, expr->token, lhs, rhs);
    }
    break;
  default: assert(false); break;
  }
  return cond;
}

Vector *parse_args(Token **ptoken) {
  Vector *args = NULL;
  Token *token;
  if ((token = match(TK_RPAR)) == NULL) {
    args = new_vector();
    for (;;) {
      Expr *arg = parse_assign();
      vec_push(args, arg);
      if ((token = match(TK_RPAR)) != NULL)
        break;
      consume(TK_COMMA, "Comma or `)` expected");
    }
  }

  *ptoken = token;
  return args;
}

const Type *get_callee_type(Expr *func) {
  const Type *type = func->type;
  if (type->kind == TY_PTR)
    type = type->pa.ptrof;
  if (type->kind != TY_FUNC)
    parse_error(func->token, "Cannot call except function");
  return type;
}

void check_funcall_args(Expr *func, Vector *args, Scope *scope, Vector *toplevel) {
  const Type *functype = get_callee_type(func);

  const Vector *param_types = functype->func.param_types;  // <const Type*>
  bool vaargs = functype->func.vaargs;
  if (param_types != NULL) {
    int argc = args != NULL ? args->len : 0;
    int paramc = param_types->len;
    if (!(argc == paramc ||
          (vaargs && argc >= paramc)))
      parse_error(func->token, "function `%.*s' expect %d arguments, but %d", func->var.name->bytes, func->var.name->chars, paramc, argc);
  }

  if (args != NULL) {
    int paramc = param_types != NULL ? param_types->len : -1;
    for (int i = 0, len = args->len; i < len; ++i) {
      Expr *arg = args->data[i];
      if (arg->type->kind == TY_ARRAY) {
        arg = str_to_char_array_var(scope, arg, toplevel);
        arg = make_cast(array_to_ptr(arg->type), arg->token, arg, false);
      }
      if (i < paramc) {
        const Type *type = param_types->data[i];
        arg = make_cast(type, arg->token, arg, false);
      } else if (vaargs && i >= paramc) {
        const Type *type = arg->type;
        switch (type->kind) {
        case TY_FIXNUM:
          if (type->fixnum.kind < FX_INT)  // Promote variadic argument.
            arg = make_cast(&tyInt, arg->token, arg, false);
          break;
#ifndef __NO_FLONUM
        case TY_FLONUM:
          if (type->flonum.kind < FL_DOUBLE)  // Promote variadic argument.
            arg = make_cast(&tyDouble, arg->token, arg, false);
          break;
#endif
        default: break;
        }
      }
      args->data[i] = arg;
    }
  }
}

static Expr *parse_funcall(Expr *func) {
  Token *token;
  Vector *args = parse_args(&token);

  check_funcall_args(func, args, curscope, toplevel);
  return new_expr_funcall(token, func, get_callee_type(func), args);
}

static Expr *parse_array_index(const Token *token, Expr *array) {
  Expr *index = parse_expr();
  consume(TK_RBRACKET, "`]' expected");
  array = str_to_char_array_var(curscope, array, toplevel);
  return new_expr_deref(token, new_expr_addsub(EX_ADD, token, array, index, false));
}

static Expr *parse_member_access(Expr *target, Token *acctok) {
  Token *ident = consume(TK_IDENT, "`ident' expected");
  const Name *name = ident->ident;

  // Find member's type from struct info.
  const Type *type = target->type;
  if (acctok->kind == TK_DOT) {
    if (type->kind != TY_STRUCT)
      parse_error(acctok, "`.' for non struct value");
  } else {  // TK_ARROW
    if (!ptr_or_array(type)) {
      parse_error(acctok, "`->' for non pointer value");
    } else {
      type = type->pa.ptrof;
      if (type->kind != TY_STRUCT)
        parse_error(acctok, "`->' for non struct value");
    }
  }

  ensure_struct((Type*)type, ident, curscope);
  int index = var_find(type->struct_.info->members, name);
  if (index >= 0) {
    const VarInfo *member = type->struct_.info->members->data[index];
    const Type *type = qualified_type(member->type, target->type->qualifier);
    return new_expr_member(acctok, type, target, ident, index);
  } else {
    Vector *stack = new_vector();
    const VarInfo *member = search_from_anonymous(type, ident->ident, ident, stack);
    if (member == NULL)
      parse_error(ident, "`%.*s' doesn't exist in the struct", name->bytes, name->chars);
    Expr *p = target;
    for (int i = 0; i < stack->len; ++i) {
      int index = (int)(long)stack->data[i];
      const VarInfo *member = type->struct_.info->members->data[index];
      type = qualified_type(member->type, type->qualifier);
      p = new_expr_member(acctok, type, p, acctok, index);
    }
    return p;
  }
}

static void parse_enum_members(const Type *type) {
  assert(type != NULL && type->kind == TY_FIXNUM && type->fixnum.kind == FX_ENUM);
  const Type *ctype = qualified_type(type, TQ_CONST);
  int value = 0;
  for (;;) {
    Token *token = consume(TK_IDENT, "ident expected");
    if (match(TK_ASSIGN)) {
      Expr *expr = parse_const();
      value = expr->fixnum;
    }

    if (scope_find(global_scope, token->ident, NULL) != NULL) {
      parse_error(token, "`%.*s' is already defined",
                  token->ident->bytes, token->ident->chars);
    } else {
      define_enum_member(ctype, token, value);
    }
    ++value;

    if (match(TK_COMMA))
      ;
    if (match(TK_RBRACE))
      break;
  }
}

static const Type *parse_enum(void) {
  Token *ident = match(TK_IDENT);
  Type *type = ident != NULL ? find_enum(curscope, ident->ident) : NULL;
  if (match(TK_LBRACE)) {
    if (type != NULL)
      parse_error(ident, "Duplicate enum type");
    type = define_enum(curscope, ident != NULL ? ident->ident : NULL);
    if (!match(TK_RBRACE))
      parse_enum_members(type);
  } else {
    if (type == NULL)
      parse_error(ident, "Unknown enum type");
  }
  return type;
}

const Type *parse_raw_type(int *pstorage) {
  const Type *type = NULL;

  int storage = 0, qualifier = 0;
  bool is_unsigned = false;
  int long_count = 0;
  for (;;) {
    Token *tok;
    if (match(TK_UNSIGNED)) {
      is_unsigned = true;
      continue;
    }
    if (match(TK_SIGNED)) {
      is_unsigned = false;
      continue;
    }
    if (match(TK_STATIC)) {
      storage |= VS_STATIC;
      continue;
    }
    if (match(TK_EXTERN)) {
      storage |= VS_EXTERN;
      continue;
    }
    if (match(TK_TYPEDEF)) {
      storage |= VS_TYPEDEF;
      continue;
    }
    if (match(TK_CONST)) {
      qualifier |= TQ_CONST;
      continue;
    }
    if (match(TK_VOLATILE)) {
      qualifier |= TQ_VOLATILE;
      continue;
    }
    if ((tok = match(TK_LONG)) != NULL) {
      ++long_count;
      if (long_count > 2)
        parse_error(tok, "Too many `long'");
      continue;
    }

    if (type != NULL)
      break;

    Token *ident;
    if (((tok = match(TK_STRUCT)) != NULL) ||
        ((tok = match(TK_UNION)) != NULL)) {
      if (is_unsigned)
        parse_error(tok, "`unsigned' for struct/union");

      bool is_union = tok->kind == TK_UNION;
      const Name *name = NULL;
      Token *ident;
      if ((ident = match(TK_IDENT)) != NULL)
        name = ident->ident;

      StructInfo *sinfo = NULL;
      if (match(TK_LBRACE)) {  // Definition
        sinfo = parse_struct(is_union);
        if (name != NULL) {
          Scope *scope;
          StructInfo *exist = find_struct(curscope, name, &scope);
          if (exist != NULL && scope == curscope)
            parse_error(ident, "`%.*s' already defined", name->bytes, name->chars);
          define_struct(curscope, name, sinfo);
        }
      } else {
        if (name != NULL) {
          sinfo = find_struct(curscope, name, NULL);
          if (sinfo != NULL) {
            if (sinfo->is_union != is_union)
              parse_error(tok, "Wrong tag for `%.*s'", name->bytes, name->chars);
          }
        }
      }

      if (name == NULL && sinfo == NULL)
        parse_error(NULL, "Illegal struct/union usage");

      type = create_struct_type(sinfo, name, qualifier);
    } else if ((tok = match(TK_ENUM)) != NULL) {
      if (is_unsigned)
        parse_error(tok, "`unsigned' for enum");

      type = parse_enum();
    } else if ((ident = match(TK_IDENT)) != NULL) {
      type = find_typedef(curscope, ident->ident, NULL);
      if (type == NULL) {
        unget_token(ident);
      } else {
        if (is_unsigned)
          parse_error(ident, "`unsigned' for typedef");
      }
    } else if ((tok = match(TK_VOID)) != NULL) {
      if (is_unsigned)
        parse_error(tok, "`unsigned' for void");

      type = &tyVoid;
#ifndef __NO_FLONUM
    } else if (match(TK_FLOAT)) {
      type = &tyFloat;
    } else if (match(TK_DOUBLE)) {
      type = &tyDouble;
#endif
    } else {
      static const enum TokenKind kIntTypeTokens[] = {
        TK_CHAR, TK_SHORT, TK_INT,
      };
      static const enum FixnumKind kFixnumKinds[] = {
        FX_CHAR, FX_SHORT, FX_INT,
      };
      const int N = sizeof(kIntTypeTokens) / sizeof(*kIntTypeTokens);
      for (int i = 0; i < N; ++i) {
        if ((tok = match(kIntTypeTokens[i])) != NULL) {
          switch (long_count) {
          default:
            // Fallthrough
          case 0:
            type = get_fixnum_type(kFixnumKinds[i], is_unsigned, qualifier);
            break;
          case 1: case 2:
            if (i != sizeof(kIntTypeTokens) / sizeof(*kIntTypeTokens) - 1)
              parse_error(tok, "`long' can use only with `int' ");
            break;
          }
          break;
        }
      }
    }
    if (type == NULL)
      break;
  }

  if (type == NULL && (storage != 0 || is_unsigned || long_count > 0)) {
    static const enum FixnumKind kLongKinds[] = {
      FX_INT, FX_LONG, FX_LLONG,
    };
    type = get_fixnum_type(kLongKinds[long_count], is_unsigned, qualifier);
  }

  if (pstorage != NULL)
    *pstorage = storage;

  return type;
}

const Type *parse_type_modifier(const Type *type) {
  if (type == NULL)
    return NULL;

  for (;;) {
    if (match(TK_CONST))
      type = qualified_type(type, TQ_CONST);
    if (match(TK_MUL))
      type = ptrof(type);
    else
      break;
  }

  return type;
}

const Type *parse_type_suffix(const Type *type) {
  if (type == NULL)
    return NULL;

  if (!match(TK_LBRACKET))
    return type;
  size_t length = -1;
  if (match(TK_RBRACKET)) {
    // Arbitrary size.
  } else {
    const Token *tok = fetch_token();
    Expr *expr = parse_const();
    if (expr->fixnum <= 0)
      parse_error(tok, "Array size must be greater than 0, but %d", (int)expr->fixnum);
    length = expr->fixnum;
    consume(TK_RBRACKET, "`]' expected");
  }
  return arrayof(parse_type_suffix(type), length);
}

Vector *extract_varinfo_types(const Vector *params) {
  Vector *param_types = NULL;
  if (params != NULL) {
    param_types = new_vector();
    for (int i = 0, len = params->len; i < len; ++i)
      vec_push(param_types, ((VarInfo*)params->data[i])->type);
  }
  return param_types;
}

static const Type *parse_var_def_cont(const Type *type) {
  if (match(TK_LPAR)) {
    const Type *rettype = type;
    bool vaargs;
    Vector *params = parse_funparams(&vaargs);
    Vector *param_types = extract_varinfo_types(params);
    type = new_func_type(rettype, params, param_types, vaargs);
  }
  if (type->kind != TY_VOID)
    type = parse_type_suffix(type);

  return type;
}

bool parse_var_def(const Type **prawType, const Type **ptype, int *pstorage, Token **pident) {
  const Type *rawType = prawType != NULL ? *prawType : NULL;
  if (rawType == NULL) {
    rawType = parse_raw_type(pstorage);
    if (rawType == NULL)
      return false;
    if (prawType != NULL)
      *prawType = rawType;
  }

  const Type *type = parse_type_modifier(rawType);
  Token *ident;
  if (match(TK_LPAR)) {  // Funcion pointer type.
    const Type *base_type = type;
    Type *place_holder = calloc(1, sizeof(*place_holder));
    type = parse_type_modifier(place_holder);
    ident = match(TK_IDENT);
    type = parse_type_suffix(type);
    consume(TK_RPAR, "`)' expected");

    const Type *inner_type = parse_var_def_cont(base_type);
    memcpy(place_holder, inner_type, sizeof(*place_holder));
  } else {
    ident = match(TK_IDENT);
    type = parse_var_def_cont(type);
  }
  *ptype = type;
  if (pident != NULL)
    *pident = ident;
  return true;
}

const Type *parse_full_type(int *pstorage, Token **pident) {
  const Type *type;
  if (!parse_var_def(NULL, &type, pstorage, pident))
    return NULL;
  return type;
}

Vector *parse_funparams(bool *pvaargs) {
  Vector *params = NULL;
  bool vaargs = false;
  if (match(TK_RPAR)) {
    // Arbitrary funparams.
  } else {
    params = new_vector();
    for (;;) {
      if (match(TK_ELLIPSIS)) {
        vaargs = true;
        consume(TK_RPAR, "`)' expected");
        break;
      }

      const Type *type;
      int storage;
      Token *ident;
      if (!parse_var_def(NULL, &type, &storage, &ident))
        parse_error(NULL, "type expected");
      if (storage & VS_STATIC)
        parse_error(ident, "`static' for function parameter");
      if (storage & VS_EXTERN)
        parse_error(ident, "`extern' for function parameter");
      if (storage & VS_TYPEDEF)
        parse_error(ident, "`typedef' for function parameter");

      if (params->len == 0) {
        if (type->kind == TY_VOID) {  // fun(void)
          if (ident != NULL || !match(TK_RPAR))
            parse_error(NULL, "`)' expected");
          break;
        }
      } else {
        not_void(type, NULL);
      }

      // If the type is array, handle it as a pointer.
      if (type->kind == TY_ARRAY)
        type = array_to_ptr(type);

      var_add(params, ident != NULL ? ident->ident : NULL, type, storage, ident);
      if (match(TK_RPAR))
        break;
      consume(TK_COMMA, "Comma or `)' expected");
    }
  }
  *pvaargs = vaargs;
  return params;
}

// Parse struct or union definition `{...}`
static StructInfo *parse_struct(bool is_union) {
  Vector *members = new_vector();
  for (;;) {
    if (match(TK_RBRACE))
      break;

    const Type *rawType = NULL;
    for (;;) {
      const Type *type;
      int storage;
      Token *ident;
      if (!parse_var_def(&rawType, &type, &storage, &ident))
        parse_error(NULL, "type expected");
      not_void(type, NULL);
      if (type->kind == TY_STRUCT) {
        ensure_struct((Type*)type, ident, curscope);
        // Allow ident to be null for anonymous struct member.
      } else {
        if (ident == NULL)
          parse_error(NULL, "`ident' expected");
      }
      const Name *name = ident != NULL ? ident->ident : NULL;
      var_add(members, name, type, storage, ident);

      if (match(TK_COMMA))
        continue;
      consume(TK_SEMICOL, "`;' expected");
      break;
    }
  }
  return create_struct_info(members, is_union);
}

static Expr *parse_compound_literal(const Type *type) {
  Token *token = fetch_token();
  Initializer *init = parse_initializer();
  const Name *name = NULL;
  Vector *inits = NULL;
  Expr *var = NULL;

  if (is_global_scope(curscope)) {
    parse_error(token, "cannot use compound literal in global");
  } else {
    if (type->kind == TY_ARRAY)
      fix_array_size((Type*)type, init);

    name = alloc_label();
    const Token *ident = alloc_ident(name, NULL, NULL);
    scope_add(curscope, ident, type, 0);

    var = new_expr_variable(name, type, token, curscope);
    inits = assign_initial_value(var, init, NULL);
  }

  return new_expr_complit(type, token, var, inits);
}

static Expr *parse_prim(void) {
  Token *tok;
  if ((tok = match(TK_LPAR)) != NULL) {
    int storage;
    const Type *type = parse_full_type(&storage, NULL);
    if (type != NULL) {  // Compound literal
      consume(TK_RPAR, "`)' expected");
      Token *tok2 = consume(TK_LBRACE, "`{' expected");
      unget_token(tok2);
      return parse_compound_literal(type);
    } else {
      Expr *expr = parse_expr();
      consume(TK_RPAR, "No close paren");
      return expr;
    }
  }

  {
    static const struct {
      enum TokenKind tk;
      enum FixnumKind fx;
      bool is_unsigned;
    } TABLE[] = {
      {TK_CHARLIT, FX_CHAR, false},
      {TK_INTLIT, FX_INT, false},
      {TK_LONGLIT, FX_LONG, false},
      {TK_LLONGLIT, FX_LLONG, false},
      {TK_UCHARLIT, FX_CHAR, true},
      {TK_UINTLIT, FX_INT, true},
      {TK_ULONGLIT, FX_LONG, true},
      {TK_ULLONGLIT, FX_LLONG, true},
    };
    for (int i = 0, n = sizeof(TABLE) / sizeof(*TABLE); i < n; ++i) {
      if ((tok = match(TABLE[i].tk)) != NULL) {
        const Type *type = get_fixnum_type(TABLE[i].fx, TABLE[i].is_unsigned, 0);
        Fixnum fixnum = tok->fixnum;
        return new_expr_fixlit(type, tok, fixnum);
      }
    }
  }
#ifndef __NO_FLONUM
  if ((tok = match(TK_FLOATLIT)) != NULL) {
    return new_expr_flolit(&tyFloat, tok, tok->flonum);
  }
  if ((tok = match(TK_DOUBLELIT)) != NULL) {
    return new_expr_flolit(&tyDouble, tok, tok->flonum);
  }
#endif

  if ((tok = match(TK_STR)) != NULL)
    return new_expr_str(tok, tok->str.buf, tok->str.size);

  Token *ident = consume(TK_IDENT, "Number or Ident or open paren expected");
  const Name *name = ident->ident;
  {
    BuiltinExprProc *proc = table_get(&builtin_expr_ident_table, name);
    if (proc != NULL) {
      return (*proc)(ident);
    }
  }
  Scope *scope;
  VarInfo *varinfo = scope_find(curscope, name, &scope);
  const Type *type;
  if (varinfo != NULL) {
    if (varinfo->storage & VS_ENUM_MEMBER)
      return new_expr_fixlit(varinfo->type, ident, varinfo->enum_member.value);
    type = varinfo->type;
  } else {
    parse_error(ident, "undefined indentifier");
    type = &tyInt;
  }
  return new_expr_variable(name, type, ident, scope);
}

static Expr *parse_postfix(void) {
  Expr *expr = parse_prim();

  for (;;) {
    Token *tok;
    if (match(TK_LPAR))
      expr = parse_funcall(expr);
    else if ((tok = match(TK_LBRACKET)) != NULL)
      expr = parse_array_index(tok, expr);
    else if ((tok = match(TK_DOT)) != NULL || (tok = match(TK_ARROW)) != NULL)
      expr = parse_member_access(expr, tok);
    else if ((tok = match(TK_INC)) != NULL) {
      not_const(expr->type, tok);
      expr = new_expr_incdec(EX_POSTINC, tok, expr);
    } else if ((tok = match(TK_DEC)) != NULL) {
      not_const(expr->type, tok);
      expr = new_expr_incdec(EX_POSTDEC, tok, expr);
    } else
      return expr;
  }
}

static Expr *parse_sizeof(const Token *token) {
  const Type *type = NULL;
  Token *tok;
  if ((tok = match(TK_LPAR)) != NULL) {
    type = parse_full_type(NULL, NULL);
    if (type != NULL) {
      consume(TK_RPAR, "`)' expected");
    } else {
      unget_token(tok);
      Expr *expr = parse_prim();
      type = expr->type;
    }
  } else {
    Expr *expr = parse_unary();
    type = expr->type;
  }
  assert(type != NULL);
  if (type->kind == TY_STRUCT)
    ensure_struct((Type*)type, token, curscope);
  if (type->kind == TY_ARRAY) {
    if (type->pa.length == (size_t)-1) {
      // TODO: assert `export` modifier.
      parse_error(token, "size unknown");
    }
  }

  const Fixnum size = type_size(type);
  return new_expr_fixlit(&tySize, token, size);
}

static Expr *parse_unary(void) {
  Token *tok;
  if ((tok = match(TK_ADD)) != NULL) {
    Expr *expr = parse_cast_expr();
    if (!is_number(expr->type))
      parse_error(tok, "Cannot apply `+' except number types");
    if (is_const(expr))
      return expr;
    return new_expr_unary(EX_POS, expr->type, tok, expr);
  }

  if ((tok = match(TK_SUB)) != NULL) {
    Expr *expr = parse_cast_expr();
    if (!is_number(expr->type))
      parse_error(tok, "Cannot apply `-' except number types");
    if (is_const(expr)) {
#ifndef __NO_FLONUM
      if (is_flonum(expr->type)) {
        expr->flonum = -expr->flonum;
        return expr;
      }
#endif
      expr->fixnum = -expr->fixnum;
      if (expr->type->fixnum.is_unsigned) {
        size_t size = type_size(expr->type);
        if (size < sizeof(UFixnum))
          expr->fixnum &= (((UFixnum)1) << (size * CHAR_BIT)) - 1;
      }
      return expr;
    }
    return new_expr_unary(EX_NEG, expr->type, tok, expr);
  }

  if ((tok = match(TK_NOT)) != NULL) {
    Expr *expr = parse_cast_expr();
    if (!is_number(expr->type) && !ptr_or_array(expr->type))
      parse_error(tok, "Cannot apply `!' except number or pointer types");
    if (is_const(expr)) {
      switch (expr->kind) {
      case EX_FIXNUM:
        expr->fixnum = !expr->fixnum;
        break;
#ifndef __NO_FLONUM
      case EX_FLONUM:
        {
          Fixnum value = expr->fixnum == 0;
          expr = new_expr_fixlit(&tyBool, tok, value);
        }
        break;
#endif
      case EX_STR:
        {
          Fixnum value = 0;
          expr = new_expr_fixlit(&tyBool, tok, value);
        }
        break;
      default:
        assert(false);
        break;
      }
      return expr;
    }
    return make_not_cond(expr);
  }

  if ((tok = match(TK_TILDA)) != NULL) {
    Expr *expr = parse_cast_expr();
    if (!is_fixnum(expr->type->kind))
      parse_error(tok, "Cannot apply `~' except number type");
    if (is_const(expr)) {
      expr->fixnum = ~expr->fixnum;
      return expr;
    }
    return new_expr_unary(EX_BITNOT, expr->type, tok, expr);
  }

  if ((tok = match(TK_AND)) != NULL) {
    Expr *expr = parse_cast_expr();
    assert(expr->type != NULL);
    return make_refer(tok, expr);
  }

  if ((tok = match(TK_MUL)) != NULL) {
    Expr *expr = parse_cast_expr();
    const Type *type = expr->type;
    assert(type != NULL);
    switch (type->kind) {
    case TY_PTR: case TY_ARRAY:
      type = type->pa.ptrof;
      break;
    case TY_FUNC:
      break;
    default:
      parse_error(tok, "Cannot dereference raw type");
      break;
    }
    expr = str_to_char_array_var(curscope, expr, toplevel);
    return new_expr_unary(EX_DEREF, type, tok, expr);
  }

  if ((tok = match(TK_INC)) != NULL) {
    Expr *expr = parse_unary();
    not_const(expr->type, tok);
    return new_expr_incdec(EX_PREINC, tok, expr);
  }

  if ((tok = match(TK_DEC)) != NULL) {
    Expr *expr = parse_unary();
    not_const(expr->type, tok);
    return new_expr_incdec(EX_PREDEC, tok, expr);
  }

  if ((tok = match(TK_SIZEOF)) != NULL) {
    return parse_sizeof(tok);
  }

  return parse_postfix();
}

static Expr *parse_cast_expr(void) {
  Token *lpar;
  if ((lpar = match(TK_LPAR)) != NULL) {
    int storage;
    const Token *token = fetch_token();
    const Type *type = parse_full_type(&storage, NULL);
    if (type != NULL) {  // Cast
      consume(TK_RPAR, "`)' expected");

      Token *token2 = fetch_token();
      if (token2 != NULL && token2->kind == TK_LBRACE)
        return parse_compound_literal(type);

      Expr *sub = parse_cast_expr();
      check_cast(type, sub->type, is_zero(sub), true, token);
      if (sub->kind == EX_CAST) {
        sub->type = type;
        return sub;
      }
      if (is_const(sub) && type->kind != TY_VOID)
        return make_cast(type, token, sub, true);
      return sub->type->kind != TY_VOID ? new_expr_cast(type, token, sub) : sub;
    }
    unget_token(lpar);
  }
  return parse_unary();
}

static Expr *parse_mul(void) {
  Expr *expr = parse_cast_expr();

  for (;;) {
    enum ExprKind kind;
    Token *tok;
    if ((tok = match(TK_MUL)) != NULL)
      kind = EX_MUL;
    else if ((tok = match(TK_DIV)) != NULL)
      kind = EX_DIV;
    else if ((tok = match(TK_MOD)) != NULL)
      kind = EX_MOD;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_cast_expr();
    expr = new_expr_num_bop(kind, tok, lhs, rhs, false);
  }
}

static Expr *parse_add(void) {
  Expr *expr = parse_mul();

  for (;;) {
    enum ExprKind kind;
    Token *tok;
    if ((tok = match(TK_ADD)) != NULL)
      kind = EX_ADD;
    else if ((tok = match(TK_SUB)) != NULL)
      kind = EX_SUB;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_mul();
    expr = new_expr_addsub(kind, tok, lhs, rhs, false);
  }
}

static Expr *parse_shift(void) {
  Expr *expr = parse_add();

  for (;;) {
    enum ExprKind kind;
    Token *tok;
    if ((tok = match(TK_LSHIFT)) != NULL)
      kind = EX_LSHIFT;
    else if ((tok = match(TK_RSHIFT)) != NULL)
      kind = EX_RSHIFT;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_add();
    if (!is_fixnum(lhs->type->kind) ||
        !is_fixnum(rhs->type->kind))
      parse_error(tok, "Cannot use `%.*s' except numbers.", (int)(tok->end - tok->begin), tok->begin);

    if (is_const(lhs) && is_const(rhs)) {
      Fixnum value;
      if (lhs->type->fixnum.is_unsigned) {
        UFixnum lval = lhs->fixnum;
        UFixnum rval = rhs->fixnum;
        value = kind == EX_LSHIFT ? lval << rval : lval >> rval;
      } else {
        Fixnum lval = lhs->fixnum;
        Fixnum rval = rhs->fixnum;
        value = kind == EX_LSHIFT ? lval << rval : lval >> rval;
      }
      expr = new_expr_fixlit(lhs->type, tok, value);
    } else {
      expr = new_expr_bop(kind, lhs->type, tok, lhs, rhs);
    }
  }
}

static Expr *parse_cmp(void) {
  Expr *expr = parse_shift();

  for (;;) {
    enum ExprKind kind;
    Token *tok;
    if ((tok = match(TK_LT)) != NULL)
      kind = EX_LT;
    else if ((tok = match(TK_GT)) != NULL)
      kind = EX_GT;
    else if ((tok = match(TK_LE)) != NULL)
      kind = EX_LE;
    else if ((tok = match(TK_GE)) != NULL)
      kind = EX_GE;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_shift();
    expr = new_expr_cmp(kind, tok, lhs, rhs);
  }
}

static Expr *parse_eq(void) {
  Expr *expr = parse_cmp();

  for (;;) {
    enum ExprKind kind;
    Token *tok;
    if ((tok = match(TK_EQ)) != NULL)
      kind = EX_EQ;
    else if ((tok = match(TK_NE)) != NULL)
      kind = EX_NE;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_cmp();
    expr = new_expr_cmp(kind, tok, lhs, rhs);
  }
}

static Expr *parse_and(void) {
  Expr *expr = parse_eq();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_AND)) == NULL)
      return expr;

    Expr *lhs = expr, *rhs = parse_eq();
    expr = new_expr_int_bop(EX_BITAND, tok, lhs, rhs, false);
  }
}

static Expr *parse_xor(void) {
  Expr *expr = parse_and();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_HAT)) == NULL)
      return expr;

    Expr *lhs = expr, *rhs = parse_and();
    expr = new_expr_int_bop(EX_BITXOR, tok, lhs, rhs, false);
  }
}

static Expr *parse_or(void) {
  Expr *expr = parse_xor();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_OR)) == NULL)
      return expr;

    Expr *lhs = expr, *rhs = parse_xor();
    expr = new_expr_int_bop(EX_BITOR, tok, lhs, rhs, false);
  }
}

static Expr *parse_logand(void) {
  Expr *expr = parse_or();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_LOGAND)) != NULL)
      expr = new_expr_bop(EX_LOGAND, &tyBool, tok, make_cond(expr), make_cond(parse_or()));
    else
      return expr;
  }
}

static Expr *parse_logior(void) {
  Expr *expr = parse_logand();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_LOGIOR)) != NULL)
      expr = new_expr_bop(EX_LOGIOR, &tyBool, tok, make_cond(expr), make_cond(parse_logand()));
    else
      return expr;
  }
}

static const Type *to_ptr_type(const Type *type) {
  switch (type->kind) {
  case TY_ARRAY: return array_to_ptr(type);
  case TY_FUNC:  return ptrof(type);
  default:  return type;
  }
}

static const Type *choose_type(Expr *tval, Expr *fval) {
  const Type *ttype = tval->type;
  const Type *ftype = fval->type;
  ttype = to_ptr_type(ttype);
  ftype = to_ptr_type(ftype);

  if (ftype->kind == TY_ARRAY)
    ftype = array_to_ptr(ftype);

  if (same_type(ttype, ftype))
    return ttype;
  if (ttype->kind == TY_PTR) {
    if (ftype->kind == TY_PTR) {  // Both pointer type
      if (is_void_ptr(ttype))
        return ftype;
      if (is_void_ptr(ftype))
        return ttype;
    } else {
      if (can_cast(ttype, ftype, is_zero(fval), false))
        return ttype;
    }
  } else if (ftype->kind == TY_PTR) {
    return choose_type(fval, tval);  // Make ttype to pointer, and check again.
  } else if (is_number(ttype) && is_number(ftype)) {
#ifndef __NO_FLONUM
    if (is_flonum(ttype)) {
      // TODO: Choose lager one.
      //if (is_flonum(ftype)) {
      //  return ttype;
      //}
      return ttype;
    } else if (is_flonum(ftype)) {
      return ftype;
    }
#endif
    assert(is_fixnum(ttype->kind));
    assert(is_fixnum(ftype->kind));
    if (ttype->fixnum.kind > ftype->fixnum.kind)
      return ttype;
    else
      return ftype;
  }
  return NULL;
}

static Expr *parse_conditional(void) {
  Expr *expr = parse_logior();
  for (;;) {
    const Token *tok;
    if ((tok = match(TK_QUESTION)) == NULL)
      return expr;
    Expr *tval = parse_expr();
    consume(TK_COLON, "`:' expected");
    Expr *fval = parse_conditional();

    tval = str_to_char_array_var(curscope, tval, toplevel);
    fval = str_to_char_array_var(curscope, fval, toplevel);

    const Type *type;
    if (tval->type->kind == TY_VOID || fval->type->kind == TY_VOID) {
      type = &tyVoid;
    } else {
      type = choose_type(tval, fval);
      if (type == NULL)
        parse_error(tok, "lhs and rhs must be same type");
      assert(type->kind != TY_VOID);
      tval = make_cast(type, tval->token, tval, false);
      fval = make_cast(type, fval->token, fval, false);
    }
    if (is_const(expr)) {
      bool tf;
      switch (expr->kind) {
      case EX_FIXNUM:  tf = expr->fixnum != 0; break;
#ifndef __NO_FLONUM
      case EX_FLONUM:  tf = expr->flonum != 0; break;
#endif
      default:
        assert(false);
        // Fallthrough to avoid warning.
      case EX_STR:     tf = true; break;
      }
      expr = tf ? tval : fval;
    } else {
      expr = new_expr_ternary(tok, make_cond(expr), tval, fval, type);
    }
  }
}

Expr *parse_assign(void) {
  enum {
    ASSIGN,
    ADDSUB,
    MULDIV,
    FIXNUM_BOP,
    SHIFT,
  };

  static const struct {
    enum TokenKind tk;
    enum ExprKind ex;
    int mode;
  } kAssignWithOps[] = {
    { TK_ASSIGN, EX_ASSIGN, ASSIGN },
    { TK_ADD_ASSIGN, EX_ADD, ADDSUB },
    { TK_SUB_ASSIGN, EX_SUB, ADDSUB },
    { TK_MUL_ASSIGN, EX_MUL, MULDIV },
    { TK_DIV_ASSIGN, EX_DIV, MULDIV },
    { TK_MOD_ASSIGN, EX_MOD, FIXNUM_BOP },
    { TK_AND_ASSIGN, EX_BITAND, FIXNUM_BOP },
    { TK_OR_ASSIGN, EX_BITOR, FIXNUM_BOP },
    { TK_HAT_ASSIGN, EX_BITXOR, FIXNUM_BOP },
    { TK_LSHIFT_ASSIGN, EX_LSHIFT, SHIFT },
    { TK_RSHIFT_ASSIGN, EX_RSHIFT, SHIFT },
  };

  Expr *expr = parse_conditional();

  Token *tok = match(-1);
  if (tok != NULL) {
    for (int i = 0; i < (int)(sizeof(kAssignWithOps) / sizeof(*kAssignWithOps)); ++i) {
      if (tok->kind == kAssignWithOps[i].tk) {
        enum ExprKind kind = kAssignWithOps[i].ex;
        Expr *lhs = expr, *rhs = parse_assign();

        check_lval(tok, lhs, "Cannot assign");

        switch (lhs->type->kind) {
        case TY_ARRAY:
          parse_error(tok, "Cannot assign to array");
          break;
        case TY_FUNC:
          parse_error(tok, "Cannot assign to function");
          break;
        default: break;
        }

        not_const(lhs->type, tok);
        Expr *bop;
        switch (kAssignWithOps[i].mode) {
        case ASSIGN:
          rhs = str_to_char_array_var(curscope, rhs, toplevel);
          return new_expr_bop(EX_ASSIGN, lhs->type, tok, lhs, make_cast(lhs->type, tok, rhs, false));
        case ADDSUB:  bop = new_expr_addsub(kind, tok, lhs, rhs, true); break;
        case MULDIV:  bop = new_expr_num_bop(kind, tok, lhs, rhs, true); break;
        case FIXNUM_BOP:  bop = new_expr_int_bop(kind, tok, lhs, rhs, true); break;
        case SHIFT:
          {
            const Type *ltype = lhs->type;
            const Type *rtype = rhs->type;
            assert(ltype != NULL);
            assert(rtype != NULL);
            if (!is_fixnum(ltype->kind) || !is_fixnum(rtype->kind))
              parse_error(tok, "Cannot use `%.*s' except numbers.", (int)(tok->end - tok->begin), tok->begin);
            bop = new_expr_bop(kind, lhs->type, tok, lhs, rhs);
          }
          break;
        default:  assert(false); bop = NULL; break;
        }
        assert(bop->type != NULL);
        return new_expr_unary(EX_MODIFY, lhs->type, tok, bop);
      }
    }
    unget_token(tok);
  }
  return expr;
}

Expr *parse_const(void) {
  Expr *expr = parse_conditional();
  if (!(is_const(expr) && is_fixnum(expr->type->kind)))
    parse_error(expr->token, "constant value expected");
  return expr;
}

Expr *parse_expr(void) {
  Expr *expr = parse_assign();
  Expr *last = expr;
  const Token *tok;
  while ((tok = match(TK_COMMA)) != NULL) {
    Expr *next_expr = parse_assign();
    if (is_const(expr))
      expr = last = next_expr;
    else if (is_const(next_expr))
      last = next_expr;
    else
      expr = last = new_expr_bop(EX_COMMA, next_expr->type, tok, expr, next_expr);
  }
  if (expr != last)
    expr = new_expr_bop(EX_COMMA, last->type, tok, expr, last);
  return expr;
}
