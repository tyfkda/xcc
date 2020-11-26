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

static StructInfo *parse_struct(bool is_union);
static Expr *parse_cast_expr(void);
static Expr *parse_unary(void);

static void define_enum_member(Type *type, const Token *ident, int value) {
  VarInfo *varinfo = scope_add(curscope, ident, type, VF_ENUM_MEMBER | VF_CONST);
  varinfo->enum_member.value = value;
}

void not_void(const Type *type) {
  if (type->kind == TY_VOID)
    parse_error(NULL, "`void' not allowed");
}

bool same_type(const Type *type1, const Type *type2, Scope *scope) {
  for (;;) {
    if (type1->kind != type2->kind)
      return false;

    switch (type1->kind) {
    case TY_VOID:
      return true;
    case TY_FIXNUM:
      return type1->fixnum.kind == type2->fixnum.kind &&
          type1->fixnum.is_unsigned == type2->fixnum.is_unsigned;
    case TY_ARRAY:
      if (type1->pa.length != type2->pa.length)
        return false;
      // Fallthrough
    case TY_PTR:
      type1 = type1->pa.ptrof;
      type2 = type2->pa.ptrof;
      continue;
    case TY_FUNC:
      if (!same_type(type1->func.ret, type2->func.ret, scope) || type1->func.vaargs != type2->func.vaargs)
        return false;
      if (type1->func.param_types == NULL && type2->func.param_types == NULL)
        return true;
      if (type1->func.param_types == NULL || type2->func.param_types == NULL ||
          type1->func.param_types->len != type2->func.param_types->len)
        return false;
      for (int i = 0, len = type1->func.param_types->len; i < len; ++i) {
        const Type *t1 = (const Type*)type1->func.param_types->data[i];
        const Type *t2 = (const Type*)type2->func.param_types->data[i];
        if (!same_type(t1, t2, scope))
          return false;
      }
      return true;
    case TY_STRUCT:
      {
        if (type1->struct_.info != NULL) {
          if (type2->struct_.info != NULL)
            return type1->struct_.info == type2->struct_.info;
          const Type *tmp = type1;
          type1 = type2;
          type2 = tmp;
        } else if (type2->struct_.info == NULL) {
          return equal_name(type1->struct_.name, type2->struct_.name);
        }
        // Find type1 from name.
        StructInfo *sinfo = find_struct(scope, type1->struct_.name, NULL);
        if (sinfo == NULL)
          return false;
        return sinfo == type2->struct_.info;
      }
    }
  }
}

bool can_cast(const Type *dst, const Type *src, bool zero, bool is_explicit, Scope *scope) {
  if (same_type(dst, src, scope))
    return true;

  if (dst->kind == TY_VOID)
    return src->kind == TY_VOID || is_explicit;
  if (src->kind == TY_VOID)
    return false;

  switch (dst->kind) {
  case TY_FIXNUM:
    switch (src->kind) {
    case TY_FIXNUM:
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
        return can_cast(dst, src->pa.ptrof, zero, is_explicit, scope);
      break;
    case TY_ARRAY:
      if (is_explicit)
        return true;
      if (same_type(dst->pa.ptrof, src->pa.ptrof, scope) ||
          can_cast(dst, ptrof(src->pa.ptrof), zero, is_explicit, scope))
        return true;
      break;
    case TY_FUNC:
      if (is_explicit)
        return true;
      if (dst->pa.ptrof->kind == TY_FUNC) {
        const Type *ftype = dst->pa.ptrof;
        return (same_type(ftype, src, scope) ||
                (ftype->func.param_types == NULL || src->func.param_types == NULL));
      }
      break;
    default:  break;
    }
    break;
  case TY_ARRAY:
    switch (src->kind) {
    case TY_PTR:
      if (is_explicit && same_type(dst->pa.ptrof, src->pa.ptrof, scope))
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

// Returns created global variable info.
VarInfo *str_to_char_array(const Type *type, Initializer *init) {
  assert(type->kind == TY_ARRAY && is_char_type(type->pa.ptrof));
  const Token *ident = alloc_ident(alloc_label(), NULL, NULL);
  VarInfo *varinfo = scope_add(curscope, ident, type, VF_CONST | VF_STATIC);
  if (is_global_scope(curscope)) {
    Vector *decls = new_vector();
    vec_push(decls, new_vardecl(varinfo->type, ident, init, varinfo->flag));
    vec_push(toplevel, new_decl_vardecl(decls));
  }
  varinfo->global.init = init;
  return varinfo;
}

// Call before accessing struct member to ensure that struct is declared.
void ensure_struct(Type *type, const Token *token) {
  assert(type->kind == TY_STRUCT);
  if (type->struct_.info == NULL) {
    StructInfo *sinfo = find_struct(curscope, type->struct_.name, NULL);
    if (sinfo == NULL)
      parse_error(token, "Accessing unknown struct(%.*s)'s member", type->struct_.name->bytes,
                  type->struct_.name->chars);
    type->struct_.info = sinfo;
  }

  // Recursively.
  StructInfo *sinfo = type->struct_.info;
  for (int i = 0; i < sinfo->members->len; ++i) {
    VarInfo *varinfo = sinfo->members->data[i];
    if (varinfo->type->kind == TY_STRUCT)
      ensure_struct((Type*)varinfo->type, token);
  }
}

bool check_cast(const Type *dst, const Type *src, bool zero, bool is_explicit, const Token *token) {
  if (!can_cast(dst, src, zero, is_explicit, curscope)) {
    parse_error(token, "Cannot convert value from type %d to %d", src->kind, dst->kind);
    return false;
  }
  if (dst->kind == TY_ARRAY) {
    parse_error(token, "Cannot cast to array type");
    return false;
  }
  return true;
}

Expr *make_cast(const Type *type, const Token *token, Expr *sub, bool is_explicit) {
  if (type->kind == TY_VOID || sub->type->kind == TY_VOID)
    parse_error(NULL, "cannot use `void' as a value");

  if (same_type(type, sub->type, curscope))
    return sub;
  if (is_const(sub) && is_fixnum(sub->type->kind) && is_fixnum(type->kind)) {
    int bytes = type_size(type);
    if (bytes < (int)type_size(sub->type)) {
      int bits = bytes * CHAR_BIT;
      uintptr_t mask = (-1UL) << bits;
      Fixnum value = sub->fixnum;
      if (!type->fixnum.is_unsigned &&  // signed
          (value & (1UL << (bits - 1))))  // negative
        value |= mask;
      else
        value &= ~mask;
      sub->fixnum = value;
    }
    sub->type = type;
    return sub;
  }

  check_cast(type, sub->type, is_zero(sub), is_explicit, token);

  return new_expr_cast(type, token, sub);
}

const VarInfo *search_from_anonymous(const Type *type, const Name *name, const Token *ident,
                                     Vector *stack) {
  assert(type->kind == TY_STRUCT);
  ensure_struct((Type*)type, ident);

  const Vector *members = type->struct_.info->members;
  for (int i = 0, len = members->len; i < len; ++i) {
    const VarInfo *member = members->data[i];
    if (member->name != NULL) {
      if (equal_name(member->name, name)) {
        vec_push(stack, (void*)(long)i);
        return member;
      }
    } else if (member->type->kind == TY_STRUCT) {
      vec_push(stack, (void*)(intptr_t)i);
      const VarInfo *submember = search_from_anonymous(member->type, name, ident, stack);
      if (submember != NULL)
        return submember;
      vec_pop(stack);
    }
  }
  return NULL;
}

static bool cast_integers(Expr **pLhs, Expr **pRhs, bool keep_left) {
  Expr *lhs = *pLhs;
  Expr *rhs = *pRhs;
  const Type *ltype = lhs->type;
  const Type *rtype = rhs->type;
  assert(ltype != NULL);
  assert(rtype != NULL);
  if (!is_fixnum(ltype->kind)) {
    parse_error(lhs->token, "integer type expected");
    return false;
  }
  if (!is_fixnum(rtype->kind)) {
    parse_error(rhs->token, "integer type expected");
    return false;
  }

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

static Expr *new_expr_int_bop(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  if (is_const(lhs) && is_fixnum(lhs->type->kind) &&
      is_const(rhs) && is_fixnum(rhs->type->kind)) {
    intptr_t lval = lhs->fixnum;
    intptr_t rval = rhs->fixnum;
    intptr_t value;
    switch (kind) {
    case EX_MUL:     value = lval * rval; break;
    case EX_DIV:     value = lval / rval; break;
    case EX_MOD:     value = lval % rval; break;
    case EX_BITAND:  value = lval & rval; break;
    case EX_BITOR:   value = lval | rval; break;
    case EX_BITXOR:  value = lval ^ rval; break;
    default:
      assert(!"err");
      value = -1;  // Dummy
      break;
    }
    Fixnum fixnum = value;
    const Type *type = keep_left || lhs->type->fixnum.kind >= rhs->type->fixnum.kind ? lhs->type : rhs->type;
    return new_expr_fixlit(type, lhs->token, fixnum);
  }

  cast_integers(&lhs, &rhs, keep_left);
  return new_expr_bop(kind, lhs->type, tok, lhs, rhs);
}

static Expr *new_expr_addsub(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  const Type *type = NULL;
  const Type *ltype = lhs->type;
  const Type *rtype = rhs->type;
  assert(ltype != NULL);
  assert(rtype != NULL);
  if (is_fixnum(ltype->kind) && is_fixnum(rtype->kind)) {
    if (is_const(lhs) && is_const(rhs)) {
      enum FixnumKind lnt = ltype->fixnum.kind;
      enum FixnumKind rnt = rtype->fixnum.kind;
      if (lnt == FX_ENUM)
        lnt = FX_INT;
      if (rnt == FX_ENUM)
        rnt = FX_INT;

      intptr_t lval = lhs->fixnum;
      intptr_t rval = rhs->fixnum;
      intptr_t value;
      switch (kind) {
      case EX_ADD: value = lval + rval; break;
      case EX_SUB: value = lval - rval; break;
      default:
        assert(false);
        value = -1;
        break;
      }
      Fixnum fixnum = value;
      const Type *type = lnt >= rnt ? lhs->type : rhs->type;
      return new_expr_fixlit(type, lhs->token, fixnum);
    }

    cast_integers(&lhs, &rhs, keep_left);
    type = lhs->type;
  } else if (ptr_or_array(ltype)) {
    if (is_fixnum(rtype->kind)) {
      kind = kind == EX_ADD ? EX_PTRADD : EX_PTRSUB;
      type = ltype;
      if (ltype->kind == TY_ARRAY)
        type = array_to_ptr(ltype);
    } else if (kind == EX_SUB && ptr_or_array(rtype)) {
      if (ltype->kind == TY_ARRAY)
        ltype = array_to_ptr(ltype);
      if (rtype->kind == TY_ARRAY)
        rtype = array_to_ptr(rtype);
      if (!same_type(ltype, rtype, curscope))
        parse_error(tok, "Different pointer diff");
      const Fixnum elem_size = type_size(ltype->pa.ptrof);
      return new_expr_bop(EX_DIV, &tySize, tok, new_expr_bop(EX_SUB, &tySize, tok, lhs, rhs),
                          new_expr_fixlit(&tySize, tok, elem_size));
    }
  } else if (ptr_or_array(rtype)) {
    if (kind == EX_ADD && is_fixnum(ltype->kind) && !keep_left) {
      kind = EX_PTRADD;
      // Swap lhs and rhs to make lhs as a pointer.
      Expr *tmp = lhs;
      lhs = rhs;
      rhs = tmp;
      type = lhs->type;
      if (type->kind == TY_ARRAY)
        type = array_to_ptr(type);
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
    if (!can_cast(lt, rt, is_zero(rhs), false, curscope))
      parse_error(tok, "Cannot compare pointer to other types");
    if (rt->kind != TY_PTR)
      rhs = make_cast(lhs->type, rhs->token, rhs, false);
  } else {
    if (!cast_integers(&lhs, &rhs, false))
      parse_error(tok, "Cannot compare except numbers");
  }
  return new_expr_bop(kind, &tyBool, tok, lhs, rhs);
}

//

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

static Expr *parse_funcall(Expr *func) {
  Token *token;
  Vector *args = parse_args(&token);
  const Type *functype;
  if (!((functype = func->type)->kind == TY_FUNC ||
        (func->type->kind == TY_PTR && (functype = func->type->pa.ptrof)->kind == TY_FUNC)))
    parse_error(func->token, "Cannot call except funtion");

  Vector *param_types = functype->func.param_types;  // <const Type*>
  bool vaargs = functype->func.vaargs;
  if (param_types != NULL) {
    int argc = args != NULL ? args->len : 0;
    int paramc = param_types->len;
    if (!(argc == paramc ||
          (vaargs && argc >= paramc)))
      parse_error(token, "function `%.*s' expect %d arguments, but %d", func->var.name->bytes, func->var.name->chars, paramc, argc);
  }

  if (args != NULL && param_types != NULL) {
    int paramc = param_types->len;
    for (int i = 0, len = args->len; i < len; ++i) {
      Expr *arg = args->data[i];
      if (i < param_types->len) {
        const Type *type = param_types->data[i];
        arg = make_cast(type, arg->token, arg, false);
      } else if (vaargs && i >= paramc) {
        const Type *type = arg->type;
        if (type->kind == TY_FIXNUM && type->fixnum.kind < FX_INT)  // Promote variadic argument.
          arg = make_cast(&tyInt, arg->token, arg, false);
      }
      if (arg->type->kind == TY_ARRAY)
        arg = make_cast(array_to_ptr(arg->type), arg->token, arg, false);
      args->data[i] = arg;
    }
  }

  return new_expr_funcall(token, func, functype, args);
}

static Expr *parse_array_index(const Token *token, Expr *array) {
  Expr *index = parse_expr();
  consume(TK_RBRACKET, "`]' expected");
  return new_expr_deref(token, new_expr_addsub(EX_ADD, token, array, index, false));
}

static Expr *parse_member_access(Expr *target, Token *acctok) {
  Token *ident = consume(TK_IDENT, "`ident' expected");
  const Name *name = ident->ident;

  // Find member's type from struct info.
  const Type *targetType = target->type;
  if (acctok->kind == TK_DOT) {
    if (targetType->kind != TY_STRUCT)
      parse_error(acctok, "`.' for non struct value");
  } else {  // TK_ARROW
    if (!ptr_or_array(targetType)) {
      parse_error(acctok, "`->' for non pointer value");
    } else {
      targetType = targetType->pa.ptrof;
      if (targetType->kind != TY_STRUCT)
        parse_error(acctok, "`->' for non struct value");
    }
  }

  ensure_struct((Type*)targetType, ident);
  int index = var_find(targetType->struct_.info->members, name);
  if (index >= 0) {
    const VarInfo *member = targetType->struct_.info->members->data[index];
    return new_expr_member(acctok, member->type, target, ident, index);
  } else {
    Vector *stack = new_vector();
    const VarInfo *member = search_from_anonymous(targetType, ident->ident, ident, stack);
    if (member == NULL)
      parse_error(ident, "`%.*s' doesn't exist in the struct", name->bytes, name->chars);
    Expr *p = target;
    const Type *type = targetType;
    for (int i = 0; i < stack->len; ++i) {
      int index = (int)(long)stack->data[i];
      const VarInfo *member = type->struct_.info->members->data[index];
      type = member->type;
      p = new_expr_member(acctok, type, p, acctok, index);
    }
    return p;
  }
}

static void parse_enum_members(Type *type) {
  assert(type != NULL && type->kind == TY_FIXNUM && type->fixnum.kind == FX_ENUM);
  int value = 0;
  for (;;) {
    Token *token = consume(TK_IDENT, "ident expected");
    if (match(TK_ASSIGN)) {
      Expr *expr = parse_const();
      if (!(is_const(expr) && is_fixnum(expr->type->kind)))
        parse_error(expr->token, "const expected for enum");
      value = expr->fixnum;
    }

    if (scope_find(global_scope, token->ident, NULL) != NULL) {
      parse_error(token, "`%.*s' is already defined",
                  token->ident->bytes, token->ident->chars);
    } else {
      define_enum_member(type, token, value);
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

const Type *parse_raw_type(int *pflag) {
  static const Type *kLongTypes[] = {
    &tyInt, &tyLong, &tyLLong,
  };
  static const Type *kUnsignedLongTypes[] = {
    &tyUnsignedInt, &tyUnsignedLong, &tyUnsignedLLong,
  };

  const Type *type = NULL;

  int flag = 0;
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
    if (match(TK_CONST)) {
      flag |= VF_CONST;
      continue;
    }
    if (match(TK_STATIC)) {
      flag |= VF_STATIC;
      continue;
    }
    if (match(TK_EXTERN)) {
      flag |= VF_EXTERN;
      continue;
    }
    if (match(TK_VOLATILE)) {
      flag |= VF_VOLATILE;
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

      Type *stype = malloc(sizeof(*type));
      stype->kind = TY_STRUCT;
      stype->struct_.name = name;
      stype->struct_.info = sinfo;
      type = stype;
    } else if ((tok = match(TK_ENUM)) != NULL) {
      if (is_unsigned)
        parse_error(tok, "`unsigned' for enum");

      type = parse_enum();
    } else if ((ident = match(TK_IDENT)) != NULL) {
      type = find_typedef(curscope, ident->ident);
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
    } else {
      static const enum TokenKind kIntTypeTokens[] = {
        TK_CHAR, TK_SHORT, TK_INT,
      };
      static const Type *kTypes[] = {
        &tyChar, &tyShort, &tyInt,
      };
      static const Type *kUnsignedTypes[] = {
        &tyUnsignedChar, &tyUnsignedShort, &tyUnsignedInt, &tyUnsignedLong,
      };
      const int N = sizeof(kIntTypeTokens) / sizeof(*kIntTypeTokens);
      for (int i = 0; i < N; ++i) {
        if ((tok = match(kIntTypeTokens[i])) != NULL) {
          switch (long_count) {
          default:
            // Fallthrough
          case 0:
            type = (is_unsigned ? kUnsignedTypes : kTypes)[i];
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

  if (type == NULL && (flag != 0 || is_unsigned || long_count > 0)) {
    type = is_unsigned ? kUnsignedLongTypes[long_count] : kLongTypes[long_count];
  }

  if (pflag != NULL)
    *pflag = flag;

  return type;
}

const Type *parse_type_modifier(const Type *type) {
  if (type == NULL)
    return NULL;

  for (;;) {
    if (match(TK_CONST)) {
      // TODO: Reflect to the type.
      ;
    }
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
    if (!(is_const(expr) && is_fixnum(expr->type->kind)))
      parse_error(tok, "constant expected");
    if (expr->fixnum <= 0)
      parse_error(tok, "Array size must be greater than 0, but %d", (int)expr->fixnum);
    length = expr->fixnum;
    consume(TK_RBRACKET, "`]' expected");
  }
  return arrayof(parse_type_suffix(type), length);
}

Vector *extract_varinfo_types(Vector *params) {
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

bool parse_var_def(const Type **prawType, const Type **ptype, int *pflag, Token **pident) {
  const Type *rawType = prawType != NULL ? *prawType : NULL;
  if (rawType == NULL) {
    rawType = parse_raw_type(pflag);
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

const Type *parse_full_type(int *pflag, Token **pident) {
  const Type *type;
  if (!parse_var_def(NULL, &type, pflag, pident))
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
      int flag;
      Token *ident;
      if (!parse_var_def(NULL, &type, &flag, &ident))
        parse_error(NULL, "type expected");
      if (flag & VF_STATIC)
        parse_error(ident, "`static' for function parameter");
      if (flag & VF_EXTERN)
        parse_error(ident, "`extern' for function parameter");

      if (params->len == 0) {
        if (type->kind == TY_VOID) {  // fun(void)
          if (ident != NULL || !match(TK_RPAR))
            parse_error(NULL, "`)' expected");
          break;
        }
      } else {
        not_void(type);
      }

      // If the type is array, handle it as a pointer.
      if (type->kind == TY_ARRAY)
        type = array_to_ptr(type);

      var_add(params, ident != NULL ? ident->ident : NULL, type, flag, ident);
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
      int flag;
      Token *ident;
      if (!parse_var_def(&rawType, &type, &flag, &ident))
        parse_error(NULL, "type expected");
      not_void(type);
      if (type->kind == TY_STRUCT) {
        ensure_struct((Type*)type, ident);
        // Allow ident to be null for anonymous struct member.
      } else {
        if (ident == NULL)
          parse_error(NULL, "`ident' expected");
      }
      const Name *name = ident != NULL ? ident->ident : NULL;
      var_add(members, name, type, flag, ident);

      if (match(TK_COMMA))
        continue;
      consume(TK_SEMICOL, "`;' expected");
      break;
    }
  }
  return create_struct(members, is_union);
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
    int flag;
    const Type *type = parse_full_type(&flag, NULL);
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
    const Type *type = NULL;
    if ((tok = match(TK_CHARLIT)) != NULL)
      type = &tyChar;
    else if ((tok = match(TK_INTLIT)) != NULL)
      type = &tyInt;
    else if ((tok = match(TK_LONGLIT)) != NULL)
      type = &tyLong;
    else if ((tok = match(TK_LLONGLIT)) != NULL)
      type = &tyLLong;
    else if ((tok = match(TK_UCHARLIT)) != NULL)
      type = &tyUnsignedChar;
    else if ((tok = match(TK_UINTLIT)) != NULL)
      type = &tyUnsignedInt;
    else if ((tok = match(TK_ULONGLIT)) != NULL)
      type = &tyUnsignedLong;
    else if ((tok = match(TK_ULLONGLIT)) != NULL)
      type = &tyUnsignedLLong;
    if (type != NULL) {
      Fixnum fixnum = tok->fixnum;
      return new_expr_fixlit(type, tok, fixnum);
    }
  }
  if ((tok = match(TK_STR)) != NULL)
    return new_expr_str(tok, tok->str.buf, tok->str.size);

  Token *ident = consume(TK_IDENT, "Number or Ident or open paren expected");
  const Name *name = ident->ident;
  Scope *scope;
  VarInfo *varinfo = scope_find(curscope, name, &scope);
  const Type *type;
  if (varinfo != NULL) {
    if (varinfo->flag & VF_ENUM_MEMBER)
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
    else if ((tok = match(TK_INC)) != NULL)
      expr = new_expr_incdec(EX_POSTINC, tok, expr);
    else if ((tok = match(TK_DEC)) != NULL)
      expr = new_expr_incdec(EX_POSTDEC, tok, expr);
    else
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
    ensure_struct((Type*)type, token);
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
    if (!is_fixnum(expr->type->kind))
      parse_error(tok, "Cannot apply `+' except number types");
    if (is_const(expr))
      return expr;
    return new_expr_unary(EX_POS, expr->type, tok, expr);
  }

  if ((tok = match(TK_SUB)) != NULL) {
    Expr *expr = parse_cast_expr();
    if (!is_fixnum(expr->type->kind))
      parse_error(tok, "Cannot apply `-' except number types");
    if (is_const(expr)) {
      expr->fixnum = -expr->fixnum;
      return expr;
    }
    return new_expr_unary(EX_NEG, expr->type, tok, expr);
  }

  if ((tok = match(TK_NOT)) != NULL) {
    Expr *expr = parse_cast_expr();
    if (!is_fixnum(expr->type->kind) && !ptr_or_array(expr->type))
      parse_error(tok, "Cannot apply `!' except number or pointer types");
    if (is_const(expr)) {
      switch (expr->kind) {
      case EX_FIXNUM:
        expr->fixnum = !expr->fixnum;
        break;
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
    return new_expr_unary(EX_NOT, &tyBool, tok, expr);
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
    check_referable(tok, expr, "Cannot take reference");
    return new_expr_unary(EX_REF, ptrof(expr->type), tok, expr);
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
    return new_expr_unary(EX_DEREF, type, tok, expr);
  }

  if ((tok = match(TK_INC)) != NULL) {
    Expr *expr = parse_unary();
    return new_expr_incdec(EX_PREINC, tok, expr);
  }

  if ((tok = match(TK_DEC)) != NULL) {
    Expr *expr = parse_unary();
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
    int flag;
    const Token *token = fetch_token();
    const Type *type = parse_full_type(&flag, NULL);
    if (type != NULL) {  // Cast
      consume(TK_RPAR, "`)' expected");

      Token *token2 = fetch_token();
      if (token2 != NULL && token2->kind == TK_LBRACE) {
        return parse_compound_literal(type);
      } else {
        Expr *sub = parse_cast_expr();
        check_cast(type, sub->type, is_zero(sub), true, token);
        if (is_const(sub) && type->kind != TY_VOID)
          return make_cast(type, token, sub, true);
        else
          return sub->type->kind != TY_VOID ? new_expr_cast(type, token, sub) : sub;
      }
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
    expr = new_expr_int_bop(kind, tok, lhs, rhs, false);
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
      intptr_t lval = lhs->fixnum;
      intptr_t rval = rhs->fixnum;
      intptr_t value = kind == EX_LSHIFT ? lval << rval : lval >> rval;
      Fixnum fixnum = value;
      expr = new_expr_fixlit(lhs->type, tok, fixnum);
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
      expr = new_expr_bop(EX_LOGAND, &tyBool, tok, expr, parse_or());
    else
      return expr;
  }
}

static Expr *parse_logior(void) {
  Expr *expr = parse_logand();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_LOGIOR)) != NULL)
      expr = new_expr_bop(EX_LOGIOR, &tyBool, tok, expr, parse_logand());
    else
      return expr;
  }
}

static const Type *choose_type(Expr *tval, Expr *fval) {
  const Type *ttype = tval->type;
  const Type *ftype = fval->type;
  if (ttype->kind == TY_ARRAY)
    ttype = array_to_ptr(ttype);
  if (ftype->kind == TY_ARRAY)
    ftype = array_to_ptr(ftype);

  if (same_type(ttype, ftype, curscope))
    return ttype;
  if (ttype->kind == TY_PTR) {
    if (ftype->kind == TY_PTR) {  // Both pointer type
      if (is_void_ptr(ttype))
        return ftype;
      if (is_void_ptr(ftype))
        return ttype;
    } else {
      if (can_cast(ttype, ftype, is_zero(fval), false, curscope))
        return ttype;
    }
  } else if (ftype->kind == TY_PTR) {
    return choose_type(fval, tval);  // Make ttype to pointer, and check again.
  } else if (is_fixnum(ttype->kind) && is_fixnum(ftype->kind)) {
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

    const Type *type = choose_type(tval, fval);
    if (type == NULL)
      parse_error(tok, "lhs and rhs must be same type");

    if (type->kind == TY_VOID) {
      assert(tval->type->kind == TY_VOID);
      assert(fval->type->kind == TY_VOID);
    } else {
      tval = make_cast(type, tval->token, tval, false);
      fval = make_cast(type, fval->token, fval, false);
    }
    expr = new_expr_ternary(tok, expr, tval, fval, type);
  }
}

Expr *parse_assign(void) {
  enum {
    ASSIGN,
    ADDSUB,
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
    { TK_MUL_ASSIGN, EX_MUL, FIXNUM_BOP },
    { TK_DIV_ASSIGN, EX_DIV, FIXNUM_BOP },
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

        Expr *bop;
        switch (kAssignWithOps[i].mode) {
        case ASSIGN:
          return new_expr_bop(EX_ASSIGN, lhs->type, tok, lhs, make_cast(lhs->type, tok, rhs, false));
        case ADDSUB:  bop = new_expr_addsub(kind, tok, lhs, rhs, true); break;
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
  return parse_conditional();
}

Expr *parse_expr(void) {
  Expr *expr = parse_assign();
  const Token *tok;
  while ((tok = match(TK_COMMA)) != NULL) {
    Expr *next_expr = parse_assign();
    expr = new_expr_bop(EX_COMMA, next_expr->type, tok, expr, next_expr);
  }
  return expr;
}
