#include "../../config.h"
#include "parser.h"

#include <assert.h>
#include <stdbool.h>

#include "ast.h"
#include "fe_misc.h"
#include "initializer.h"
#include "lexer.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

static Table builtin_expr_ident_table;

extern bool parsing_stmt;

static Expr *used_as_value(Expr *expr) {
#ifndef __NO_BITFIELD
  if (expr->kind == EX_MEMBER) {
    const MemberInfo *minfo = expr->member.info;
    if (minfo->bitfield.width > 0) {
      mark_var_used(expr);
      Type *type = get_fixnum_type(minfo->bitfield.base_kind, minfo->type->fixnum.is_unsigned, 0);
      Expr *ptr = make_cast(ptrof(type), expr->token, make_refer(expr->token, expr), true);
      Expr *load = new_expr_deref(NULL, ptr);
      expr = extract_bitfield_value(load, minfo);
    }
  }
#endif
  mark_var_used(expr);
  return expr;
}

void add_builtin_expr_ident(const char *str, BuiltinExprProc *proc) {
  const Name *name = alloc_name(str, NULL, false);
  table_put(&builtin_expr_ident_table, name, proc);
}

Vector *parse_args(Token **ptoken) {
  Vector *args = new_vector();
  Token *token;
  if ((token = match(TK_RPAR)) == NULL) {
    for (;;) {
      Expr *arg = parse_assign();
      vec_push(args, arg);
      if ((token = match(TK_RPAR)) != NULL)
        break;
      if (!consume(TK_COMMA, "`,' or `)` expected"))
        break;
    }
  }

  *ptoken = token;
  return args;
}

static Expr *parse_member_access(Expr *target, Token *acctok) {
  Token *ident = consume(TK_IDENT, "member name expected");
  target = used_as_value(target);

  // Find member's type from struct info.
  Type *type = target->type;
  switch (acctok->kind) {
  case TK_DOT:
    if (type->kind != TY_STRUCT) {
      parse_error(PE_NOFATAL, acctok, "`.' for non struct value");
      // Suppose `dot` is mistakenly used instead of `arrow`, continue parsing for pointer type.
      if (!(ptr_or_array(type) && (type = type->pa.ptrof, type->kind == TY_STRUCT)))
        return target;
    }
    break;
  case TK_ARROW:
    if (!ptr_or_array(type)) {
      parse_error(PE_NOFATAL, acctok, "`->' for non pointer value");
      // Suppose `arrow` is mistakenly used instead of `dot`, continue parsing for struct type.
      if (type->kind != TY_STRUCT)
        return target;  // Error is already reported in above, so return here.
    } else {
      type = type->pa.ptrof;
    }
    if (type->kind != TY_STRUCT) {
      parse_error(PE_NOFATAL, acctok, "`->' for non struct value");
      return target;
    }
    break;
  default: assert(false); break;
  }
  if (ident == NULL)
    return target;

  if (!ensure_struct(type, ident, curscope))
    return new_expr_fixlit(&tyInt, acctok, 0);  // TODO

  int index = find_struct_member(type->struct_.info, ident->ident);
  if (index >= 0) {
    const MemberInfo *minfo = &type->struct_.info->members[index];
    Type *type = acctok->kind == TK_DOT ? qualified_type(minfo->type, target->type->qualifier)
                                        : minfo->type;
    return new_expr_member(acctok, type, target, ident->ident, minfo);
  } else {
    Vector *stack = new_vector();
    const MemberInfo *member = search_from_anonymous(type, ident->ident, ident, stack);
    if (member == NULL) {
      parse_error(PE_NOFATAL, ident, "`%.*s' doesn't exist in the struct", NAMES(ident->ident));
      return target;
    }
    Expr *p = target;
    Token *tok = acctok;
    for (int i = 0; i < stack->len; ++i) {
      int index = VOIDP2INT(stack->data[i]);
      const MemberInfo *minfo = &type->struct_.info->members[index];
      type = qualified_type(minfo->type, type->qualifier);
      const Name *member_name = NULL;
      if (i == stack->len - 1) {  // Last one must be specified member.
        assert(equal_name(minfo->name, ident->ident));
        member_name = ident->ident;
      }
      p = new_expr_member(tok, type, p, member_name, minfo);
      if (tok->kind != TK_DOT)
        tok = alloc_token(TK_DOT, acctok->line, acctok->begin, acctok->end);
    }
    return p;
  }
}

Type *parse_var_def(Type **prawType, int *pstorage, Token **pident) {
  Type *rawType = prawType != NULL ? *prawType : NULL;
  if (rawType == NULL) {
    rawType = parse_raw_type(pstorage);
    if (rawType == NULL)
      return NULL;
    if (prawType != NULL)
      *prawType = rawType;
  }

  if (rawType->kind == TY_AUTO)
    return parse_direct_declarator(rawType, pident);

  return parse_declarator(rawType, pident);
}

static Expr *parse_compound_literal(Type *type) {
  Token *token = fetch_token();
  Initializer *init = parse_initializer();

  if (type->kind == TY_ARRAY)
    type = fix_array_size(type, init, token);

  Expr *var = alloc_tmp_var(curscope, type);
  Vector *inits = NULL;
  if (is_global_scope(curscope)) {
    // Global variable initializer is flattened in `check_vardecl()`
    const Name *name = var->var.name;
    VarInfo *varinfo = scope_find(curscope, name, NULL);
    assert(varinfo != NULL);
    varinfo->storage |= VS_STATIC;
    varinfo->global.init = init;
  } else {
    init = flatten_initializer(type, init);
    inits = assign_initial_value(var, init, NULL);
  }

  return new_expr_complit(type, token, var, inits, init);
}

static Expr *parse_generic(void) {
  consume(TK_LPAR, "`(' expected");
  Expr *target = parse_assign();
  target = used_as_value(target);
  consume(TK_COMMA, "`,' expected");

  Vector *types = new_vector();
  Vector *exprs = new_vector();
  Expr *default_value = NULL;
  do {
    Token *tok = fetch_token();
    Type *type = NULL;
    if (!match(TK_DEFAULT)) {
      int storage;
      Token *ident;
      type = parse_var_def(NULL, &storage, &ident);
      if (type == NULL) {
        parse_error(PE_NOFATAL, tok, "type expected");
        break;
      }
      if (storage != 0)
        parse_error(PE_NOFATAL, tok, "storage class specifier not allowed");
      if (ident != NULL)
        parse_error(PE_NOFATAL, tok, "identifier not allowed");
    }
    consume(TK_COLON, "`:' expected");
    Expr *expr = parse_assign();
    if (type == NULL) {
      if (default_value != NULL)
        parse_error(PE_NOFATAL, tok, "multiple default values");
      default_value = expr;
    } else {
      // TODO: Type duplication check.
      vec_push(types, type);
      vec_push(exprs, expr);
    }
  } while (match(TK_COMMA));
  consume(TK_RPAR, "`)' expected");

  Type *type = target->type;
  if (type->kind == TY_ARRAY)
    type = array_to_ptr(type);
  for (int i = 0; i < types->len; ++i) {
    Type *t = types->data[i];
    if (same_type(t, type))
      return exprs->data[i];
  }
  if (default_value == NULL) {
    parse_error(PE_NOFATAL, target->token, "no matching type found");
    default_value = exprs->data[0];
  }
  return default_value;
}

// Pratt parser.

typedef enum {
  PREC_NONE,
  PREC_COMMA,    // ,
  PREC_ASSIGN,   // =
  PREC_TERNARY,  // ?:
  PREC_LOGIOR,   // ||
  PREC_LOGAND,   // &&
  PREC_BITOR,    // |
  PREC_BITXOR,   // ^
  PREC_BITAND,   // &
  PREC_EQ,       // == !=
  PREC_CMP,      // < > <= >=
  PREC_SHIFT,    // << >>
  PREC_TERM,     // + -
  PREC_FACTOR,   // * /
  PREC_POSTFIX,  // ++ -- . -> [] ()
} Precedence;

typedef Expr *(*ParsePrefixFn)(Token *token);
typedef Expr *(*ParseInfixFn)(Expr *lhs, Token *token);

typedef struct {
  ParsePrefixFn prefix;
  ParseInfixFn infix;
  Precedence precedence;
} ParseRule;

static const ParseRule *get_rule(enum TokenKind kind);

static Expr *parse_precedence(Precedence precedence) {
  Token *previous = match(-1);
  ParsePrefixFn prefixRule = get_rule(previous->kind)->prefix;
  if (prefixRule == NULL) {
    parse_error(PE_NOFATAL, previous, "expression expected");
    unget_token(previous);
    return NULL;
  }

  Expr *expr = prefixRule(previous);

  for (;;) {
    Token *current = fetch_token();
    const ParseRule *rule = get_rule(current->kind);
    assert(rule != NULL);
    if (rule->precedence < precedence)
      break;
    ParseInfixFn infixRule = rule->infix;
    assert(infixRule != NULL);
    expr = infixRule(expr, match(-1));
  }
  return expr;
}

static Expr *literal(Token *tok) {
  switch (tok->kind) {
  case TK_INTLIT:
    {
      int flag = tok->fixnum.flag;
      int long_count = flag & TKF_LONG_MASK;
      enum FixnumKind fx;
      bool u = flag & TKF_UNSIGNED;
      if (flag & TKF_CHAR) {
#ifndef __NO_WCHAR
        static const enum FixnumKind kCharTable[] = { FX_CHAR, FX_INT };
        assert(long_count < (int)ARRAY_SIZE(kCharTable));
        fx = kCharTable[long_count];
#else
        fx = FX_CHAR;
#endif
      } else {
        static const enum FixnumKind kFxTable[] = { FX_INT, FX_LONG, FX_LLONG };
        assert(long_count < (int)ARRAY_SIZE(kFxTable));
        fx = kFxTable[long_count];

        // Check auto upgrade.
        UFixnum value = tok->fixnum.value;
        while (fx < FX_LLONG) {
          Type *type = get_fixnum_type(fx, u, 0);
          int bits = type_size(type) * TARGET_CHAR_BIT - (u ? 0 : 1);
          if ((value <= (((UFixnum)1U << bits) - 1U)) &&
              (u || (Fixnum)value >= -((Fixnum)1 << (bits - 1))))
            break;
          if (!u && value <= ((UFixnum)1U << (bits + 1)) - 1U &&
              (tok->fixnum.flag & TKF_KIND_MASK) != 0) {
            u = true;
            break;
          }
          ++fx;
        }
        if (!u && fx >= FX_LLONG) {
          // Check auto upgrade to unsigned.
          Type *type = get_fixnum_type(fx, u, 0);
          int bits = type_size(type) * TARGET_CHAR_BIT - (u ? 0 : 1);
          if (value > (((UFixnum)1 << bits) - 1))
            u = true;
        }
      }
      Type *type = get_fixnum_type(fx, u, 0);
      return new_expr_fixlit(type, tok, tok->fixnum.value);
    }
#ifndef __NO_FLONUM
  case TK_FLOATLIT: case TK_DOUBLELIT: case TK_LDOUBLELIT:
    {
      static Type *kTypes[] = {&tyFloat, &tyDouble, &tyLDouble};
      return new_expr_flolit(kTypes[tok->kind - TK_FLOATLIT], tok, tok->flonum);
    }
#endif
  case TK_STR:  return string_expr(tok, tok->str.buf, tok->str.len, tok->str.kind);
  default: assert(false); return NULL;  // Unreachable.
  }
}

static Expr *variable(Token *ident) {
  const Name *name = ident->ident;
  BuiltinExprProc *proc = table_get(&builtin_expr_ident_table, name);
  if (proc != NULL)
    return (*proc)(ident);
  Scope *scope;
  VarInfo *varinfo = scope_find(curscope, name, &scope);
  Type *type;
  if (varinfo != NULL) {
    if (varinfo->storage & VS_ENUM_MEMBER)
      return new_expr_fixlit(varinfo->type, ident, varinfo->enum_member.value);
    type = varinfo->type;
  } else {
    parse_error(PE_NOFATAL, ident, "`%.*s' undeclared", NAMES(ident->ident));
    type = &tyInt;
    scope = curscope;
    add_var_to_scope(scope, ident, type, VS_USED);
  }
  return new_expr_variable(name, type, ident, scope);
}

static Expr *unary(Token *tok) {
  Expr *expr = parse_precedence(PREC_POSTFIX);
  expr = used_as_value(expr);

  enum TokenKind kind = tok->kind;
  switch (kind) {
  case TK_ADD: case TK_SUB:
    {
      Type *type = expr->type;
      if (!is_number(type)) {
        parse_error(PE_NOFATAL, tok, "Cannot apply `%c' except number types", *tok->begin);
        return expr;
      }

      if (is_fixnum(type->kind)) {
        expr = promote_to_int(expr);
        type = expr->type;
      }
      if (is_const(expr)) {
        if (kind == TK_SUB) {
#ifndef __NO_FLONUM
          if (is_flonum(type)) {
            expr->flonum = -expr->flonum;
            return expr;
          }
#endif
          assert(is_fixnum(type->kind));
          expr->fixnum = wrap_value(-expr->fixnum, type_size(type), type->fixnum.is_unsigned);
          expr->type = type;
        }
        return expr;
      }
      return new_expr_unary(kind + (EX_POS - TK_ADD), type, tok, expr);
    }
  case TK_NOT:
    if (!is_number(expr->type) && !ptr_or_array(expr->type)) {
      parse_error(PE_NOFATAL, tok, "Cannot apply `!' except number or pointer types");
      return new_expr_fixlit(&tyBool, tok, false);
    }
    return make_not_expr(tok, expr);
  case TK_TILDA:
    if (!is_fixnum(expr->type->kind)) {
      parse_error(PE_NOFATAL, tok, "Cannot apply `~' except integer");
      return new_expr_fixlit(&tyInt, expr->token, 0);
    }
    expr = promote_to_int(expr);
    if (is_const(expr)) {
      Type *type = expr->type;
      expr->fixnum = wrap_value(~expr->fixnum, type_size(type), type->fixnum.is_unsigned);
      return expr;
    }
    return new_expr_unary(EX_BITNOT, expr->type, tok, expr);
  case TK_MUL:
    {
      Type *type = expr->type;
      assert(type != NULL);
      switch (type->kind) {
      case TY_PTR: case TY_ARRAY:
        type = type->pa.ptrof;
        break;
      case TY_FUNC:
        break;
      default:
        parse_error(PE_NOFATAL, tok, "Cannot dereference raw type");
        return expr;
      }
      expr = str_to_char_array_var(curscope, expr);
      return new_expr_unary(EX_DEREF, type, tok, expr);
    }
  case TK_AND:
#ifndef __NO_BITFIELD
    if (expr->kind == EX_MEMBER) {
      const MemberInfo *minfo = expr->member.info;
      if (minfo->bitfield.active)
        parse_error(PE_NOFATAL, tok, "Cannot take reference for bitfield");
    }
#endif
    expr = str_to_char_array_var(curscope, expr);
    return make_refer(tok, expr);
  default: assert(false); return NULL;  // Unreachable.
  }
}

static Expr *unary_inc(Token *tok) {
  Expr *expr = parse_precedence(PREC_POSTFIX);
  mark_var_used(expr);

  enum TokenKind kind = tok->kind;
  switch (kind) {
  case TK_INC: case TK_DEC:
    not_const(expr->type, tok);
    return incdec_of(kind + (EX_PREINC - TK_INC), expr, tok);
  default: assert(false); return NULL;  // Unreachable.
  }
}

static Expr *binary(Expr *lhs, Token *tok) {
  const ParseRule* rule = get_rule(tok->kind);
  assert(rule != NULL);
  Expr *rhs = parse_precedence(rule->precedence + 1);

  lhs = used_as_value(lhs);
  rhs = used_as_value(rhs);

  enum TokenKind kind = tok->kind;
  switch (kind) {
  case TK_ADD: case TK_SUB:
    return new_expr_addsub(kind + (EX_ADD - TK_ADD), tok, lhs, rhs);
  case TK_MUL: case TK_DIV: case TK_MOD:
    return new_expr_num_bop(kind + (EX_MUL - TK_MUL), tok, lhs, rhs);
  case TK_LSHIFT: case TK_RSHIFT:
    if (!is_fixnum(lhs->type->kind) ||
        !is_fixnum(rhs->type->kind))
      parse_error(PE_NOFATAL, tok, "Cannot use `%.*s' except numbers.", (int)(tok->end - tok->begin), tok->begin);

    lhs = promote_to_int(lhs);
    if (is_const(lhs) && is_const(rhs)) {
      Type *type = lhs->type;
      if (type->fixnum.kind < FX_INT)
        type = get_fixnum_type(FX_INT, type->fixnum.is_unsigned, type->qualifier);
      Fixnum value;
      if (type->fixnum.is_unsigned) {
        UFixnum lval = lhs->fixnum;
        UFixnum rval = rhs->fixnum;
        value = kind == TK_LSHIFT ? lval << rval : lval >> rval;
      } else {
        Fixnum lval = lhs->fixnum;
        Fixnum rval = rhs->fixnum;
        value = kind == TK_LSHIFT ? lval << rval : lval >> rval;
      }
      value = wrap_value(value, type_size(type), type->fixnum.is_unsigned);
      return new_expr_fixlit(type, tok, value);
    } else {
      return new_expr_bop(kind + (EX_LSHIFT - TK_LSHIFT), lhs->type, tok, lhs, rhs);
    }
  case TK_AND: case TK_OR: case TK_HAT:
    return new_expr_int_bop(kind + (EX_BITAND - TK_AND), tok, lhs, rhs);
  case TK_EQ: case TK_NE: case TK_LT: case TK_LE: case TK_GE: case TK_GT:
    return new_expr_cmp(kind + (EX_EQ - TK_EQ), tok, lhs, rhs);
  case TK_LOGAND: case TK_LOGIOR:
    lhs = make_cond(lhs);
    rhs = make_cond(rhs);
    if (lhs->kind == EX_FIXNUM)
      return (kind == TK_LOGAND ? lhs->fixnum == 0 : lhs->fixnum != 0) ? lhs : rhs;
    else
      return new_expr_bop(kind + (EX_LOGAND - TK_LOGAND), &tyBool, tok, lhs, rhs);
  case TK_COMMA:
    if (is_const(lhs))
      return rhs;
    return new_expr_bop(EX_COMMA, rhs->type, tok, lhs, rhs);

  default: assert(false); return NULL;  // Unreachable.
  }
}

static Expr *ternary(Expr *expr, Token *tok) {
  const ParseRule* rule = get_rule(tok->kind);
  assert(rule != NULL);
  Expr *tval = parse_expr();

  consume(TK_COLON, "`:' expected");
  Expr *fval = parse_precedence(rule->precedence);

  expr = used_as_value(expr);
  tval = used_as_value(tval);
  fval = used_as_value(fval);

  tval = str_to_char_array_var(curscope, tval);
  fval = str_to_char_array_var(curscope, fval);

  Type *type;
  type = choose_ternary_result_type(tval, fval);
  if (type == NULL) {
    parse_error(PE_NOFATAL, tok, "lhs and rhs must be same type");
    type = tval->type;  // Dummy to continue.
  } else {
    if (is_fixnum(type->kind) && type->fixnum.kind < FX_INT)
      type = &tyInt;
    if (type->kind != TY_VOID) {
      tval = make_cast(type, tval->token, tval, false);
      fval = make_cast(type, fval->token, fval, false);
    }
  }

  expr = make_cond(expr);
  if (expr->kind == EX_FIXNUM)
    return expr->fixnum != 0 ? tval : fval;
  else
    return new_expr_ternary(tok, expr, tval, fval, type);
}

static Expr *assign(Expr *lhs, Token *tok) {
  const ParseRule* rule = get_rule(tok->kind);
  assert(rule != NULL);
  Expr *rhs = parse_precedence(rule->precedence);  // Without +1 for right associativity.

  rhs = used_as_value(rhs);

  check_lval(tok, lhs, "Cannot assign");
  not_const(lhs->type, tok);

  switch (lhs->type->kind) {
  case TY_ARRAY:
  case TY_FUNC:
    parse_error(PE_NOFATAL, tok, "Cannot assign to %s", lhs->type->kind == TY_ARRAY ? "array" : "function");
    return rhs;
  default: break;
  }

  if (tok->kind == TK_ASSIGN) {
    rhs = str_to_char_array_var(curscope, rhs);
    if (lhs->type->kind == TY_STRUCT) {  // Struct assignment requires same type.
      if (!same_type_without_qualifier(lhs->type, rhs->type, true))
        parse_error(PE_NOFATAL, tok, "Cannot assign to incompatible struct");
    } else {  // Otherwise, cast-ability required.
      rhs = make_cast(lhs->type, tok, rhs, false);
    }
#ifndef __NO_BITFIELD
    if (lhs->kind == EX_MEMBER) {
      const MemberInfo *minfo = lhs->member.info;
      if (minfo->bitfield.active)
        return assign_to_bitfield(tok, lhs, rhs, minfo);
    }
#endif
    return new_expr_bop(EX_ASSIGN, lhs->type, tok, lhs, rhs);
  }

  mark_var_used(lhs);
  return transform_assign_with(tok, lhs, rhs);
}

static Expr *postfix(Expr *expr, Token *tok) {
  mark_var_used(expr);

  switch (tok->kind) {
  case TK_INC: case TK_DEC:
    not_const(expr->type, tok);
    return incdec_of(tok->kind + (EX_POSTINC - TK_INC), expr, tok);
  case TK_DOT: case TK_ARROW:
    return parse_member_access(expr, tok);
  default: assert(false); return NULL;  // Unreachable.
  }
}

static Expr *array_index(Expr *expr, Token *token) {
  Expr *index = parse_expr();
  consume(TK_RBRACKET, "`]' expected");
  expr = used_as_value(expr);
  index = used_as_value(index);
  expr = str_to_char_array_var(curscope, expr);
  index = str_to_char_array_var(curscope, index);
  if (!ptr_or_array(expr->type)) {
    if (!ptr_or_array(index->type)) {
      parse_error(PE_NOFATAL, expr->token, "array or pointer required for `['");
      return expr;
    }
    Expr *tmp = expr;
    expr = index;
    index = tmp;
  }
  if (!is_fixnum(index->type->kind)) {
    parse_error(PE_NOFATAL, index->token, "int required for `['");
  } else {
    expr = new_expr_addsub(EX_ADD, token, expr, index);
  }
  return new_expr_deref(token, expr);
}

static Expr *grouping(void) {
  Expr *expr = parse_expr();
  consume(TK_RPAR, "Expect ')' after expression.");
  return expr;
}

static Expr *lparen(Token *tok) {
  int storage;
  Token *token = fetch_token();
  Type *type = parse_var_def(NULL, &storage, NULL);
  if (type == NULL) {
    if (!match(TK_LBRACE))
      return grouping();

    // gcc extension: Statement expression ({})
    Stmt *block = parse_block(tok, NULL);
    consume(TK_RPAR, "`)' expected");
    Vector *stmts = block->block.stmts;
    if (stmts->len > 0) {
      Stmt *last = stmts->data[stmts->len - 1];
      if (last->kind == ST_EXPR)
        last->expr = used_as_value(last->expr);
    }
    return new_expr_block(block);
  }

  // (type)
  consume(TK_RPAR, "`)' expected");

  if (storage & (VS_EXTERN | VS_STATIC | VS_TYPEDEF | VS_INLINE))
    parse_error(PE_NOFATAL, token, "storage specifier not allowed");
  if (type->kind == TY_AUTO) {
    parse_error(PE_NOFATAL, token, "auto type not allowed");
    type = &tyInt;  // Dummy
  }

  if (fetch_token()->kind == TK_LBRACE)
    return parse_compound_literal(type);

  // Cast expression.
  Expr *sub = parse_precedence(PREC_POSTFIX);
  sub = used_as_value(sub);
  sub = str_to_char_array_var(curscope, sub);
  check_cast(type, sub->type, is_zero(sub), true, token);

  // Do not reduce cast expression using `make_cast`
  // because it ignores `(int)x = 1`.

  if (type->kind != TY_VOID && is_const(sub))
    return make_cast(type, token, sub, true);
  return sub->type->kind != TY_VOID ? new_expr_cast(type, token, sub) : sub;
}

static Expr *funcall(Expr *func, Token *tok) {
  Token *dummy;
  Vector *args = parse_args(&dummy);

  mark_var_used_for_func(func);
  for (int i = 0; i < args->len; ++i)
    args->data[i] = used_as_value(args->data[i]);

  check_funcall_args(func, args, curscope);
  Type *functype = get_callee_type(func->type);
  if (functype == NULL) {
    parse_error(PE_NOFATAL, func->token, "Cannot call except function");
    return func;
  }

  Type *rettype = functype->func.ret;
  ensure_struct(rettype, tok, curscope);

  if (func->kind == EX_VAR && is_global_scope(func->var.scope)) {
    VarInfo *varinfo = scope_find(func->var.scope, func->var.name, NULL);
    assert(varinfo != NULL);
    if (satisfy_inline_criteria(varinfo))
      return new_expr_inlined(tok, varinfo->ident->ident, rettype, args,
                              embed_inline_funcall(varinfo));
    // Not inlined.
    if (varinfo->storage & VS_INLINE)
      varinfo->storage |= VS_EXTERN;  // To emit inline function.
  }

  Expr *funcall = new_expr_funcall(tok, functype, func, args);
  return simplify_funcall(funcall);
}

static Expr *size_align_of(Token *token) {
  Type *type = NULL;
  const Token *tok;
  if ((tok = match(TK_LPAR)) != NULL) {
    type = parse_var_def(NULL, NULL, NULL);
    if (type != NULL) {
      consume(TK_RPAR, "`)' expected");
#ifndef __NO_VLA
      Expr *vla_size = calc_vla_size(type);
      if (vla_size != NULL)
        return new_expr_bop(EX_COMMA, &tySize, token, vla_size, calc_type_size(type));
#endif
    } else {
      unget_token((Token*)tok);
      Expr *expr = parse_precedence(PREC_POSTFIX);
      not_bitfield_member(expr);
      expr = used_as_value(expr);
      type = expr->type;
      tok = expr->token;
    }
  } else {
    Expr *expr = parse_precedence(PREC_POSTFIX);
    not_bitfield_member(expr);
    type = expr->type;
    tok = expr->token;
  }
  assert(type != NULL);
  if (!ensure_struct(type, tok, curscope))
    return new_expr_fixlit(&tySize, token, 1);  // Dummy
#ifndef __NO_VLA
  if (ptr_or_array(type) && type->pa.vla != NULL)
    return calc_type_size(type);
#endif
  switch (type->kind) {
  case TY_ARRAY:
    if (type->pa.length == -1) {
      parse_error(PE_NOFATAL, tok, "size unknown");
      type->pa.length = 1;  // Continue parsing.
    }
    assert(type->pa.length >= 0);
    break;
  case TY_AUTO:
    parse_error(PE_NOFATAL, tok, "size unknown");
    type = &tyInt;  // Dummy.
    break;
  default: break;
  }

  const Fixnum size = token->kind == TK_SIZEOF ? type_size(type) : align_size(type);
  return new_expr_fixlit(&tySize, token, size);
}

static Expr *generic(Token *tok) {
  UNUSED(tok);
  return parse_generic();
}

static const ParseRule *get_rule(enum TokenKind kind) {
  static const ParseRule kRules[] = {
    [TK_LPAR]          = {lparen,   funcall,   PREC_POSTFIX},
    [TK_INC]           = {unary_inc, postfix,   PREC_POSTFIX},
    [TK_DEC]           = {unary_inc, postfix,   PREC_POSTFIX},
    [TK_DOT]           = {NULL,     postfix,   PREC_POSTFIX},
    [TK_ARROW]         = {NULL,     postfix,   PREC_POSTFIX},
    [TK_LBRACKET]      = {NULL, array_index,   PREC_POSTFIX},

    [TK_MUL]           = {unary,    binary,    PREC_FACTOR},
    [TK_DIV]           = {NULL,     binary,    PREC_FACTOR},
    [TK_MOD]           = {NULL,     binary,    PREC_FACTOR},

    [TK_ADD]           = {unary,    binary,    PREC_TERM},
    [TK_SUB]           = {unary,    binary,    PREC_TERM},

    [TK_LSHIFT]        = {NULL,     binary,    PREC_SHIFT},
    [TK_RSHIFT]        = {NULL,     binary,    PREC_SHIFT},

    [TK_LT]            = {NULL,     binary,    PREC_CMP},
    [TK_LE]            = {NULL,     binary,    PREC_CMP},
    [TK_GE]            = {NULL,     binary,    PREC_CMP},
    [TK_GT]            = {NULL,     binary,    PREC_CMP},

    [TK_EQ]            = {NULL,     binary,    PREC_EQ},
    [TK_NE]            = {NULL,     binary,    PREC_EQ},

    [TK_AND]           = {unary,    binary,    PREC_BITAND},
    [TK_HAT]           = {NULL,     binary,    PREC_BITXOR},
    [TK_OR]            = {NULL,     binary,    PREC_BITOR},

    [TK_LOGAND]        = {NULL,     binary,    PREC_LOGAND},
    [TK_LOGIOR]        = {NULL,     binary,    PREC_LOGIOR},

    [TK_QUESTION]      = {NULL,     ternary,   PREC_TERNARY},

    [TK_ASSIGN]        = {NULL,     assign,    PREC_ASSIGN},
    [TK_ADD_ASSIGN]    = {NULL,     assign,    PREC_ASSIGN},
    [TK_SUB_ASSIGN]    = {NULL,     assign,    PREC_ASSIGN},
    [TK_MUL_ASSIGN]    = {NULL,     assign,    PREC_ASSIGN},
    [TK_DIV_ASSIGN]    = {NULL,     assign,    PREC_ASSIGN},
    [TK_MOD_ASSIGN]    = {NULL,     assign,    PREC_ASSIGN},
    [TK_AND_ASSIGN]    = {NULL,     assign,    PREC_ASSIGN},
    [TK_OR_ASSIGN]     = {NULL,     assign,    PREC_ASSIGN},
    [TK_HAT_ASSIGN]    = {NULL,     assign,    PREC_ASSIGN},
    [TK_LSHIFT_ASSIGN] = {NULL,     assign,    PREC_ASSIGN},
    [TK_RSHIFT_ASSIGN] = {NULL,     assign,    PREC_ASSIGN},

    [TK_COMMA]         = {NULL,     binary,    PREC_COMMA},

    [TK_NOT]           = {unary},
    [TK_TILDA]         = {unary},

    [TK_INTLIT]        = {literal},
    [TK_STR]           = {literal},
    [TK_FLOATLIT]      = {literal},
    [TK_DOUBLELIT]     = {literal},
    [TK_LDOUBLELIT]    = {literal},

    [TK_IDENT]         = {variable},

    [TK_SIZEOF]        = {size_align_of},
    [TK_ALIGNOF]       = {size_align_of},
    [TK_GENERIC]       = {generic},

    [TK_EOF]           = {NULL},
  };

  if (kind >= ARRAY_SIZE(kRules))
    kind = TK_EOF;
  return &kRules[kind];
}

Expr *parse_expr(void) {
  parsing_stmt = false;
  return parse_precedence(PREC_COMMA);
}

Expr *parse_assign(void) {
  parsing_stmt = false;
  return parse_precedence(PREC_ASSIGN);
}

Expr *parse_const_fixnum(void) {
  parsing_stmt = false;
  Expr *expr = parse_precedence(PREC_LOGIOR);
  if (expr == NULL)
    return NULL;  // Error is already reported.
  if (is_const(expr) && is_fixnum(expr->type->kind))
    return expr;
  parse_error(PE_NOFATAL, expr->token, "constant integer expected");
  return new_expr_fixlit(&tyInt, expr->token, 1);
}
