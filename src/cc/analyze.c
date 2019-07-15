#include "expr.h"

#include <assert.h>
#include <string.h>

#include "lexer.h"
#include "type.h"
#include "util.h"
#include "var.h"

static const Type *tyNumTable[] = { &tyChar, &tyShort, &tyInt, &tyLong, &tyEnum };

Expr *new_expr_sizeof(const Token *token, const Type *type, Expr *sub) {
  Expr *expr = new_expr(EX_SIZEOF, &tySize, token);
  expr->u.sizeof_.type = type;
  expr->u.sizeof_.sub = sub;
  return expr;
}

// num +|- num
static Expr *add_num(enum ExprType exprType, const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  const Type *ltype = lhs->valType;
  const Type *rtype = rhs->valType;
  assert(ltype->type == TY_NUM && rtype->type == TY_NUM);
  enum NumType lnt = ltype->u.numtype;
  enum NumType rnt = rtype->u.numtype;
  if (lnt == NUM_ENUM)
    lnt = NUM_INT;
  if (rnt == NUM_ENUM)
    rnt = NUM_INT;

  const Type *type;
  if (lnt >= rnt || keep_left) {
    type = tyNumTable[lnt];
    rhs = new_expr_cast(type, rhs->token, rhs, false);
  } else {
    type = tyNumTable[rnt];
    lhs = new_expr_cast(type, lhs->token, lhs, false);
  }
  return new_expr_bop(exprType, type, tok, lhs, rhs);
}

// pointer +|- num
static Expr *add_ptr_num(enum ExprType exprType, const Token *token, Expr *ptr, Expr *num) {
  const Type *ptr_type = ptr->valType;
  if (ptr_type->type == TY_ARRAY)
    ptr_type = array_to_ptr(ptr_type);
  return new_expr_bop(exprType, ptr_type, token, ptr,
                      new_expr_bop(EX_MUL, &tySize, token,
                                   new_expr_cast(&tySize, token, num, false),
                                   new_expr_sizeof(token, ptr_type->u.pa.ptrof, NULL)));
}

Expr *add_expr(const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  const Type *ltype = lhs->valType;
  const Type *rtype = rhs->valType;
  //if (ltype->type == TY_ENUM)
  //  ltype = &tyInt;
  //if (rtype->type == TY_ENUM)
  //  rtype = &tyInt;

  if (is_number(ltype->type)) {
    if (same_type(ltype, rtype))
      return new_expr_bop(EX_ADD, ltype, tok, lhs, rhs);
    if (is_number(rtype->type))
      return add_num(EX_ADD, tok, lhs, rhs, keep_left);
  }

  switch (ltype->type) {
  case TY_NUM:
    switch (rtype->type) {
    case TY_PTR: case TY_ARRAY:
      if (!keep_left)
        return add_ptr_num(EX_ADD, tok, rhs, lhs);
      break;
    default:
      break;
    }
    break;

  case TY_PTR: case TY_ARRAY:
    switch (rtype->type) {
    case TY_NUM:
      return add_ptr_num(EX_ADD, tok, lhs, rhs);
    default:
      break;
    }
    break;

  default:
    break;
  }

  parse_error(tok, "Illegal `+'");
  return NULL;
}

static Expr *diff_ptr(const Token *tok, Expr *lhs, Expr *rhs) {
  if (!same_type(lhs->valType, rhs->valType))
    parse_error(tok, "Different pointer diff");
  const Type *elem_type = lhs->valType;
  if (elem_type->type == TY_PTR)
    elem_type = elem_type->u.pa.ptrof;
  return new_expr_bop(EX_DIV, &tySize, tok,
                      new_expr_bop(EX_SUB, &tySize, tok, lhs, rhs),
                      new_expr_sizeof(tok, elem_type, NULL));
}

static Expr *sub_expr(const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  if (is_number(lhs->valType->type)) {
    if (same_type(lhs->valType, rhs->valType))
      return new_expr_bop(EX_SUB, lhs->valType, tok, lhs, rhs);
    if (is_number(rhs->valType->type))
      return add_num(EX_SUB, tok, lhs, rhs, keep_left);
  }

  switch (lhs->valType->type) {
  case TY_PTR:
    switch (rhs->valType->type) {
    case TY_NUM:
      return add_ptr_num(EX_SUB, tok, lhs, rhs);
    case TY_PTR: case TY_ARRAY:
      return diff_ptr(tok, lhs, rhs);
    default:
      break;
    }
    break;

  case TY_ARRAY:
    switch (rhs->valType->type) {
    case TY_PTR: case TY_ARRAY:
      return diff_ptr(tok, lhs, rhs);
    default:
      break;
    }
    break;

  default:
    break;
  }

  parse_error(tok, "Illegal `-'");
  return NULL;
}

static bool cast_numbers(Expr **pLhs, Expr **pRhs, bool keep_left) {
  if (!is_number((*pLhs)->valType->type) ||
      !is_number((*pRhs)->valType->type))
    return false;

  enum NumType ltype = (*pLhs)->valType->u.numtype;
  enum NumType rtype = (*pRhs)->valType->u.numtype;
  if (ltype == NUM_ENUM)
    ltype = NUM_INT;
  if (rtype == NUM_ENUM)
    rtype = NUM_INT;
  if (ltype != rtype) {
    if (ltype > rtype || keep_left)
      *pRhs = new_expr_cast((*pLhs)->valType, (*pRhs)->token, *pRhs, false);
    else if (ltype < rtype)
      *pLhs = new_expr_cast((*pRhs)->valType, (*pLhs)->token, *pLhs, false);
  }
  return true;
}

static bool member_access_recur(const Type *type, const Token *ident, Vector *stack) {
  assert(type->type == TY_STRUCT);
  ensure_struct((Type*)type, ident);
  const char *name = ident->u.ident;

  Vector *lvars = type->u.struct_.info->members;
  for (int i = 0, len = lvars->len; i < len; ++i) {
    VarInfo *info = (VarInfo*)lvars->data[i];
    if (info->name != NULL) {
      if (strcmp(info->name, name) == 0) {
        vec_push(stack, (void*)(long)i);
        return true;
      }
    } else if (info->type->type == TY_STRUCT) {
      vec_push(stack, (void*)(long)i);
      bool res = member_access_recur(info->type, ident, stack);
      if (res)
        return true;
      //vec_pop(stack);
      --stack->len;
    }
  }
  return false;
}

static void analyze_cmp(Expr *expr) {
  Expr *lhs = expr->u.bop.lhs, *rhs = expr->u.bop.rhs;
  if (lhs->valType->type == TY_PTR || rhs->valType->type == TY_PTR) {
    const Type *lt = lhs->valType, *rt = rhs->valType;
    if (lt->type != TY_PTR) {
      const Type *tmp = lt;
      lt = rt;
      rt = tmp;
    }
    if (!can_cast(lt, rt, rhs, false))
      parse_error(expr->token, "Cannot compare pointer to other types");
    if (rt->type != TY_PTR) {
      if (lt == lhs->valType)
        expr->u.bop.rhs = new_expr_cast(lhs->valType, expr->token, rhs, false);
      else
        expr->u.bop.lhs = new_expr_cast(rhs->valType, expr->token, lhs, false);
    }
  } else {
    if (!cast_numbers(&expr->u.bop.lhs, &expr->u.bop.rhs, false))
      parse_error(expr->token, "Cannot compare except numbers");
  }
}

// Traverse expr to check semantics and determine value type.
Expr *analyze_expr(Expr *expr, bool keep_left) {
  switch (expr->type) {
  // Literals
  case EX_NUM:
  case EX_STR:
    assert(expr->valType != NULL);
    break;

  case EX_VARREF:
    {
      const char *name = expr->u.varref.ident;
      const Type *type = NULL;
      bool global = false;
      if (curscope != NULL) {
        VarInfo *varinfo = scope_find(curscope, name);
        if (varinfo != NULL) {
          if (varinfo->flag & VF_STATIC) {
            // Replace local variable reference to global.
            name = varinfo->u.l.label;
            expr = new_expr_varref(name, varinfo->type, true, expr->token);
          } else {
            type = varinfo->type;
          }
        }
      }
      if (type == NULL) {
        VarInfo *varinfo = find_global(name);
        if (varinfo != NULL) {
          global = true;
          type = varinfo->type;
          if (type->type == TY_NUM && type->u.numtype == NUM_ENUM) {
            // Enum value is embeded directly.
            assert(varinfo->u.g.init->type == vSingle);
            return varinfo->u.g.init->u.single;
          }
        }
      }
      if (type == NULL)
        parse_error(expr->token, "Undefined `%s'", name);
      expr->valType = type;
      expr->u.varref.global = global;
    }
    break;

  // Binary operators
  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
  case EX_DIV:
  case EX_MOD:
  case EX_BITAND:
  case EX_BITOR:
  case EX_BITXOR:
  case EX_LSHIFT:
  case EX_RSHIFT:
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
  case EX_LOGAND:
  case EX_LOGIOR:
  case EX_ASSIGN:
    expr->u.bop.lhs = analyze_expr(expr->u.bop.lhs, false);
    expr->u.bop.rhs = analyze_expr(expr->u.bop.rhs, false);
    assert(expr->u.bop.lhs->valType != NULL);
    assert(expr->u.bop.rhs->valType != NULL);

    switch (expr->type) {
    case EX_ADD:
      return add_expr(expr->token, expr->u.bop.lhs, expr->u.bop.rhs, keep_left);
    case EX_SUB:
      return sub_expr(expr->token, expr->u.bop.lhs, expr->u.bop.rhs, keep_left);
    case EX_MUL:
    case EX_DIV:
    case EX_MOD:
    case EX_BITAND:
    case EX_BITOR:
    case EX_BITXOR:
      if (!cast_numbers(&expr->u.bop.lhs, &expr->u.bop.rhs, keep_left))
        parse_error(expr->token, "Cannot use `%d' except numbers.", expr->type);

      if (is_const(expr->u.bop.lhs) && is_const(expr->u.bop.rhs)) {
        Expr *lhs = expr->u.bop.lhs, *rhs = expr->u.bop.rhs;
        intptr_t lval = lhs->u.num.ival;
        intptr_t rval = rhs->u.num.ival;
        intptr_t value;
        switch (expr->type) {
        case EX_MUL:
          value = lval * rval;
          break;
        case EX_DIV:
          value = lval / rval;
          break;
        case EX_MOD:
          value = lval % rval;
          break;
        case EX_BITAND:
          value = lval & rval;
          break;
        case EX_BITOR:
          value = lval | rval;
          break;
        case EX_BITXOR:
          value = lval ^ rval;
          break;
        default:
          assert(!"err");
          value = -1;  // Dummy
          break;
        }
        Num num = {value};
        const Type *type = lhs->valType->u.numtype >= rhs->valType->u.numtype ? lhs->valType : rhs->valType;
        return new_expr_numlit(type, lhs->token, &num);
      }

      expr->valType = expr->u.bop.lhs->valType;
      break;

    case EX_LSHIFT:
    case EX_RSHIFT:
      {
        enum eType t;
        if (!is_number(t = expr->u.bop.lhs->valType->type) ||
            !is_number(t = expr->u.bop.rhs->valType->type))
          parse_error(expr->token, "Cannot use `%d' except numbers.", t);

        if (is_const(expr->u.bop.lhs) && is_const(expr->u.bop.rhs)) {
          intptr_t lval = expr->u.bop.lhs->u.num.ival;
          intptr_t rval = expr->u.bop.rhs->u.num.ival;
          intptr_t value = expr->type == EX_LSHIFT ? lval << rval : lval >> rval;
          Num num = {value};
          return new_expr_numlit(expr->u.bop.lhs->valType, expr->u.bop.lhs->token, &num);
        }

        expr->valType = expr->u.bop.lhs->valType;
      }
      break;

    case EX_EQ:
    case EX_NE:
    case EX_LT:
    case EX_GT:
    case EX_LE:
    case EX_GE:
      analyze_cmp(expr);
      break;

    case EX_LOGAND:
    case EX_LOGIOR:
      break;

    case EX_ASSIGN:
      expr->valType = expr->u.bop.lhs->valType;
      expr->u.bop.rhs = new_expr_cast(expr->valType, expr->token, expr->u.bop.rhs, false);
      break;

    default:
      fprintf(stderr, "expr type=%d\n", expr->type);
      assert(!"analyze not handled!");
      break;
    }
    break;

  // Unary operators
  case EX_POS:
  case EX_NEG:
  case EX_NOT:
  case EX_PREINC:
  case EX_PREDEC:
  case EX_POSTINC:
  case EX_POSTDEC:
  case EX_REF:
  case EX_DEREF:
  case EX_CAST:
  case EX_ASSIGN_WITH:
    expr->u.unary.sub = analyze_expr(expr->u.unary.sub, expr->type == EX_ASSIGN_WITH);
    assert(expr->u.unary.sub->valType != NULL);

    switch (expr->type) {
    case EX_POS:
      if (!is_number(expr->u.unary.sub->valType->type))
        parse_error(expr->token, "Cannot apply `+' except number types");
      return expr->u.unary.sub;

    case EX_NEG:
      if (!is_number(expr->u.unary.sub->valType->type))
        parse_error(expr->token, "Cannot apply `-' except number types");
      expr->valType = expr->u.unary.sub->valType;
      break;

    case EX_NOT:
      switch (expr->u.unary.sub->valType->type) {
      case TY_NUM:
      case TY_PTR:
      case TY_ARRAY:
        break;
      default:
        parse_error(expr->token, "Cannot apply `!' except number or pointer types");
        break;
      }
      break;

    case EX_PREINC:
    case EX_PREDEC:
    case EX_POSTINC:
    case EX_POSTDEC:
      expr->valType = expr->u.unary.sub->valType;
      break;

    case EX_REF:
      expr->valType = ptrof(expr->u.unary.sub->valType);
      break;

    case EX_DEREF:
      {
        Expr *sub = expr->u.unary.sub;
        if (sub->valType->type != TY_PTR && sub->valType->type != TY_ARRAY)
          parse_error(expr->token, "Cannot dereference raw type");
        expr->valType = sub->valType->u.pa.ptrof;
      }
      break;

    case EX_ASSIGN_WITH:
      expr->valType = expr->u.unary.sub->u.bop.lhs->valType;
      break;

    case EX_CAST:
      {
        Expr *sub = expr->u.unary.sub;
        if (same_type(expr->valType, sub->valType))
          return sub;
        check_cast(expr->valType, sub->valType, sub, true);
      }
      break;

    default:
      fprintf(stderr, "expr type=%d\n", expr->type);
      assert(!"analyze not handled!");
      break;
    }
    break;

  case EX_TERNARY:
    expr->u.ternary.cond = analyze_expr(expr->u.ternary.cond, false);
    expr->u.ternary.tval = analyze_expr(expr->u.ternary.tval, false);
    expr->u.ternary.fval = analyze_expr(expr->u.ternary.fval, false);
    {
      const Type *ttype = expr->u.ternary.tval->valType;
      const Type *ftype = expr->u.ternary.fval->valType;
      if (same_type(ttype, ftype)) {
        expr->valType = ttype;
      } else if (is_void_ptr(ttype) && ftype->type == TY_PTR) {
        expr->valType = ftype;
      } else if (is_void_ptr(ftype) && ttype->type == TY_PTR) {
        expr->valType = ttype;
      } else {
        parse_error(NULL, "lhs and rhs must be same type");
      }
    }
    break;

  case EX_MEMBER:  // x.member or x->member
    {
      Expr *target = expr->u.member.target;
      expr->u.member.target = target = analyze_expr(target, false);
      assert(target->valType != NULL);

      const Token *acctok = expr->u.member.acctok;
      const Token *ident = expr->u.member.ident;
      const char *name = ident->u.ident;

      // Find member's type from struct info.
      const Type *targetType = target->valType;
      if (acctok->type == TK_DOT) {
        if (targetType->type != TY_STRUCT)
          parse_error(acctok, "`.' for non struct value");
      } else {  // TK_ARROW
        if (targetType->type == TY_PTR)
          targetType = targetType->u.pa.ptrof;
        else if (targetType->type == TY_ARRAY)
          targetType = targetType->u.pa.ptrof;
        else
          parse_error(acctok, "`->' for non pointer value");
        if (targetType->type != TY_STRUCT)
          parse_error(acctok, "`->' for non struct value");
      }

      ensure_struct((Type*)targetType, ident);
      int index = var_find(targetType->u.struct_.info->members, name);
      if (index >= 0) {
        VarInfo *varinfo = (VarInfo*)targetType->u.struct_.info->members->data[index];
        expr->valType = varinfo->type;
        expr->u.member.index = index;
      } else {
        Vector *stack = new_vector();
        bool res = member_access_recur(targetType, ident, stack);
        if (!res)
          parse_error(ident, "`%s' doesn't exist in the struct", name);
        Expr *p = target;
        const Type *type = targetType;
        VarInfo *varinfo;
        for (int i = 0; i < stack->len; ++i) {
          int index = (int)(long)stack->data[i];
          varinfo = type->u.struct_.info->members->data[index];
          type = varinfo->type;
          p = new_expr_member(acctok, type, p, NULL, NULL, index);
        }
        expr = p;
      }
    }
    break;

  case EX_SIZEOF:
    {
      Expr *sub = expr->u.sizeof_.sub;
      if (sub != NULL) {
        sub = analyze_expr(sub, false);
        assert(sub->valType != NULL);
        expr->u.sizeof_.type = sub->valType;
      }
    }
    break;

  case EX_FUNCALL:
    {
      Expr *func = expr->u.funcall.func;
      Vector *args = expr->u.funcall.args;  // <Expr*>
      expr->u.funcall.func = func = analyze_expr(func, false);
      if (args != NULL) {
        for (int i = 0, len = args->len; i < len; ++i)
          args->data[i] = analyze_expr(args->data[i], false);
      }

      const Type *functype;
      if (!((functype = func->valType)->type == TY_FUNC ||
            (func->valType->type == TY_PTR && (functype = func->valType->u.pa.ptrof)->type == TY_FUNC)))
        parse_error(NULL, "Cannot call except funtion");
      expr->valType = functype->u.func.ret;

      Vector *param_types = functype->u.func.param_types;  // <const Type*>
      bool vaargs = functype->u.func.vaargs;
      if (param_types != NULL) {
        int argc = args != NULL ? args->len : 0;
        int paramc = param_types->len;
        if (!(argc == paramc ||
              (vaargs && argc >= paramc)))
          parse_error(func->token, "function `%s' expect %d arguments, but %d", func->u.varref.ident, paramc, argc);
      }

      if (args != NULL && param_types != NULL) {
        int paramc = param_types->len;
        for (int i = 0, len = args->len; i < len; ++i) {
          if (i < param_types->len) {
            Expr *arg = args->data[i];
            const Type *type = (const Type*)param_types->data[i];
            args->data[i] = new_expr_cast(type, arg->token, arg, false);
          } else if (vaargs && i >= paramc) {
            Expr *arg = args->data[i];
            const Type *type = arg->valType;
            if (type->type == TY_NUM && type->u.numtype < NUM_INT)  // Promote variadic argument.
              args->data[i] = new_expr_cast(&tyInt, arg->token, arg, false);
          }
        }
      }
    }
    break;

  case EX_COMMA:
    {
      Vector *list = expr->u.comma.list;
      int len = list->len;
      for (int i = 0; i < len; ++i)
        list->data[i] = analyze_expr(list->data[i], false);
      expr->valType = ((Expr*)list->data[len - 1])->valType;
    }
    break;

  default:
    fprintf(stderr, "expr type=%d\n", expr->type);
    assert(!"analyze not handled!");
    break;
  }

if (expr->valType == NULL) { fprintf(stderr, "expr->type=%d, ", expr->type); }
  assert(expr->valType != NULL);
  return expr;
}
