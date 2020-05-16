#include "parser.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "ast.h"
#include "lexer.h"
#include "sema.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

Scope *curscope;

static StructInfo *parse_struct(bool is_union);
static Expr *parse_cast_expr(void);
static Expr *parse_unary(void);

void not_void(const Type *type) {
  if (type->kind == TY_VOID)
    parse_error(NULL, "`void' not allowed");
}

VarInfo *add_cur_scope(const Token *ident, const Type *type, int flag) {
  if (curscope->vars == NULL)
    curscope->vars = new_vector();
  return var_add(curscope->vars, ident, type, flag);
}

static bool cast_integers(Expr **pLhs, Expr **pRhs, bool keep_left) {
  Expr *lhs = *pLhs;
  Expr *rhs = *pRhs;
  const Type *ltype = lhs->type;
  const Type *rtype = rhs->type;
  assert(ltype != NULL);
  assert(rtype != NULL);
  if (!is_number(ltype->kind)) {
    parse_error(lhs->token, "integer type expected");
    return false;
  }
  if (!is_number(rtype->kind)) {
    parse_error(rhs->token, "integer type expected");
    return false;
  }

  enum NumKind lkind = ltype->num.kind;
  enum NumKind rkind = rtype->num.kind;
  if (ltype->num.kind == NUM_ENUM) {
    ltype = &tyInt;
    lkind = NUM_INT;
  }
  if (rtype->num.kind == NUM_ENUM) {
    rtype = &tyInt;
    rkind = NUM_INT;
  }

  if (lkind != rkind) {
    if (lkind > rkind || keep_left)
      *pRhs = make_cast(ltype, rhs->token, rhs, false);
    else if (lkind < rkind)
      *pLhs = make_cast(rtype, lhs->token, lhs, false);
  }
  return true;
}

static Expr *new_expr_muldiv(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  cast_integers(&lhs, &rhs, keep_left);
  return new_expr_bop(kind, lhs->type, tok, lhs, rhs);
}

static Expr *new_expr_addsub(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  const Type *type = NULL;
  const Type *ltype = lhs->type;
  const Type *rtype = rhs->type;
  assert(ltype != NULL);
  assert(rtype != NULL);
  if (is_number(ltype->kind) && is_number(rtype->kind)) {
    cast_integers(&lhs, &rhs, keep_left);
    type = lhs->type;
  } else if (ptr_or_array(ltype)) {
    if (is_number(rtype->kind)) {
      kind = kind == EX_ADD ? EX_PTRADD : EX_PTRSUB;
      type = ltype;
      if (ltype->kind == TY_ARRAY)
        type = array_to_ptr(ltype);
    } else if (kind == EX_SUB && ptr_or_array(rtype)) {
      kind = EX_PTRSUB;
      type = &tySize;
    }
  } else if (ptr_or_array(rtype)) {
    if (kind == EX_ADD && is_number(ltype->kind) && !keep_left) {
      kind = EX_PTRADD;
      type = rtype;
      if (rtype->kind == TY_ARRAY)
        type = array_to_ptr(rtype);
    }
  }
  if (type == NULL) {
    parse_error(tok, "Cannot apply `%.*s'", (int)(tok->end - tok->begin), tok->begin);
  }
  return new_expr_bop(kind, type, tok, lhs, rhs);
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
      parse_error(token, "function `%.*s' expect %d arguments, but %d", func->variable.name->bytes, func->variable.name->chars, paramc, argc);
  }

  if (args != NULL && param_types != NULL) {
    int paramc = param_types->len;
    for (int i = 0, len = args->len; i < len; ++i) {
      if (i < param_types->len) {
        Expr *arg = args->data[i];
        const Type *type = param_types->data[i];
        args->data[i] = make_cast(type, arg->token, arg, false);
      } else if (vaargs && i >= paramc) {
        Expr *arg = args->data[i];
        const Type *type = arg->type;
        if (type->kind == TY_NUM && type->num.kind < NUM_INT)  // Promote variadic argument.
          args->data[i] = make_cast(&tyInt, arg->token, arg, false);
      }
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

static const Type *parse_enum(void) {
  Token *typeIdent = match(TK_IDENT);
  Type *type = typeIdent != NULL ? find_enum(typeIdent->ident) : NULL;
  if (match(TK_LBRACE)) {
    if (type != NULL)
      parse_error(typeIdent, "Duplicate enum type");
    type = define_enum(typeIdent != NULL ? typeIdent->ident : NULL);
    if (!match(TK_RBRACE)) {
      int value = 0;
      for (;;) {
        Token *numtok;
        Token *ident = numtok = consume(TK_IDENT, "ident expected");
        if (match(TK_ASSIGN)) {
          numtok = fetch_token();
          Expr *expr = sema_expr(parse_const());
          if (!(is_const(expr) && is_number(expr->type->kind))) {
            parse_error(numtok, "const expected for enum");
          }
          value = expr->num.ival;
        }

        intptr_t dummy;
        if (find_enum_value(ident->ident, &dummy) ||
            find_global(ident->ident) != NULL) {
          parse_error(ident, "`%.*s' is already defined", ident->ident->bytes, ident->ident->chars);
        } else {
          add_enum_member(type, ident->ident, value);
        }
        ++value;

        if (match(TK_COMMA))
          ;
        if (match(TK_RBRACE))
          break;
      }
    }
  } else {
    if (type == NULL)
      parse_error(typeIdent, "Unknown enum type");
  }
  return type;
}

const Type *parse_raw_type(int *pflag) {
  const Type *type = NULL;

  int flag = 0;
  bool is_unsigned = false;
  for (;;) {
    if (match(TK_UNSIGNED)) {
      is_unsigned = true;
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

    if (type != NULL)
      break;

    Token *tok;
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
          StructInfo *exist = find_struct(name);
          if (exist != NULL)
            parse_error(ident, "`%.*s' already defined", name->bytes, name->chars);
          define_struct(name, sinfo);
        }
      } else {
        if (name != NULL) {
          sinfo = find_struct(name);
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
      type = find_typedef(ident->ident);
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
        TK_CHAR, TK_SHORT, TK_INT, TK_LONG,
      };
      static const Type *kTypes[] = {
        &tyChar, &tyShort, &tyInt, &tyLong,
      };
      static const Type *kUnsignedTypes[] = {
        &tyUnsignedChar, &tyUnsignedShort, &tyUnsignedInt, &tyUnsignedLong,
      };
      const int N = sizeof(kIntTypeTokens) / sizeof(*kIntTypeTokens);
      for (int i = 0; i < N; ++i) {
        if (match(kIntTypeTokens[i])) {
          type = (is_unsigned ? kUnsignedTypes : kTypes)[i];
          break;
        }
      }
    }
    if (type == NULL)
      break;
  }

  if (type == NULL && (flag != 0 || is_unsigned))
    type = &tyInt;

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
    Expr *expr = sema_expr(parse_const());
    if (!(is_const(expr) && is_number(expr->type->kind)))
      parse_error(tok, "constant expected");
    if (expr->num.ival <= 0)
      parse_error(tok, "Array size must be greater than 0, but %d", (int)expr->num.ival);
    length = expr->num.ival;
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
      if (match(TK_DOTDOTDOT)) {
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
      type = array_to_ptr(type);

      var_add(params, ident, type, flag);
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
      var_add(members, ident, type, flag);

      if (match(TK_COMMA))
        continue;
      consume(TK_SEMICOL, "`;' expected");
      break;
    }
  }

  StructInfo *sinfo = malloc(sizeof(*sinfo));
  sinfo->members = members;
  sinfo->is_union = is_union;
  sinfo->size = -1;
  sinfo->align = 0;
  return sinfo;
}

static Expr *parse_prim(void) {
  Token *tok;
  if ((tok = match(TK_LPAR)) != NULL) {
    Expr *expr = parse_expr();
    consume(TK_RPAR, "No close paren");
    return new_expr_unary(EX_GROUP, expr->type, tok, expr);
  }

  {
    const Type *type = NULL;
    if ((tok = match(TK_CHARLIT)) != NULL)
      type = &tyChar;
    else if ((tok = match(TK_INTLIT)) != NULL)
      type = &tyInt;
    else if ((tok = match(TK_LONGLIT)) != NULL)
      type = &tyLong;
    else if ((tok = match(TK_UCHARLIT)) != NULL)
      type = &tyUnsignedChar;
    else if ((tok = match(TK_UINTLIT)) != NULL)
      type = &tyUnsignedInt;
    else if ((tok = match(TK_ULONGLIT)) != NULL)
      type = &tyUnsignedLong;
    if (type != NULL) {
      Num num = {tok->value};
      return new_expr_numlit(type, tok, &num);
    }
  }
  if ((tok = match(TK_STR)) != NULL)
    return new_expr_str(tok, tok->str.buf, tok->str.size);

  Token *ident = consume(TK_IDENT, "Number or Ident or open paren expected");
  const Name *name = ident->ident;
  Scope *scope = curscope;
  VarInfo *varinfo = NULL;
  const Type *type;
  if (curscope != NULL)
    varinfo = scope_find(&scope, name);
  if (varinfo == NULL)
    varinfo = find_global(name);
  if (varinfo != NULL) {
    type = varinfo->type;
  } else {
    intptr_t value;
    if (find_enum_value(name, &value)) {
      Num num = {.ival = value};
      return new_expr_numlit(&tyInt, ident, &num);
    }
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
      expr = new_expr_unary(EX_POSTINC, expr->type, tok, expr);
    else if ((tok = match(TK_DEC)) != NULL)
      expr = new_expr_unary(EX_POSTDEC, expr->type, tok, expr);
    else
      return expr;
  }
}

static Expr *parse_sizeof(const Token *token) {
  const Type *type = NULL;
  Expr *expr = NULL;
  Token *tok;
  if ((tok = match(TK_LPAR)) != NULL) {
    type = parse_full_type(NULL, NULL);
    if (type != NULL) {
      consume(TK_RPAR, "`)' expected");
    } else {
      unget_token(tok);
      expr = parse_prim();
    }
  } else {
    expr = parse_unary();
  }
  return new_expr_sizeof(token, type, expr);
}

static Expr *parse_unary(void) {
  Token *tok;
  if ((tok = match(TK_ADD)) != NULL) {
    Expr *expr = parse_cast_expr();
    switch (expr->kind) {
    case EX_NUM:
      return expr;
    default:
      return new_expr_unary(EX_POS, expr->type, tok, expr);
    }
  }

  if ((tok = match(TK_SUB)) != NULL) {
    Expr *expr = parse_cast_expr();
    switch (expr->kind) {
    case EX_NUM:
      expr->num.ival = -expr->num.ival;
      return expr;
    default:
      return new_expr_unary(EX_NEG, expr->type, tok, expr);
    }
  }

  if ((tok = match(TK_NOT)) != NULL) {
    Expr *expr = parse_cast_expr();
    return new_expr_unary(EX_NOT, &tyBool, tok, expr);
  }

  if ((tok = match(TK_TILDA)) != NULL) {
    Expr *expr = parse_cast_expr();
    return new_expr_unary(EX_BITNOT, expr->type, tok, expr);
  }

  if ((tok = match(TK_AND)) != NULL) {
    Expr *expr = parse_cast_expr();
    assert(expr->type != NULL);
    return new_expr_unary(EX_REF, ptrof(expr->type), tok, expr);
  }

  if ((tok = match(TK_MUL)) != NULL) {
    Expr *expr = parse_cast_expr();
    const Type *type = expr->type;;
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
    return new_expr_unary(EX_PREINC, expr->type, tok, expr);
  }

  if ((tok = match(TK_DEC)) != NULL) {
    Expr *expr = parse_unary();
    return new_expr_unary(EX_PREDEC, expr->type, tok, expr);
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
      Expr *sub = parse_cast_expr();
      return new_expr_cast(type, token, sub);
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
    expr = new_expr_muldiv(kind, tok, lhs, rhs, false);
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
    enum ExprKind t;
    Token *tok;
    if ((tok = match(TK_LSHIFT)) != NULL)
      t = EX_LSHIFT;
    else if ((tok = match(TK_RSHIFT)) != NULL)
      t = EX_RSHIFT;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_add();
    expr = new_expr_bop(t, lhs->type, tok, lhs, rhs);
  }
}

static Expr *parse_cmp(void) {
  Expr *expr = parse_shift();

  for (;;) {
    enum ExprKind t;
    Token *tok;
    if ((tok = match(TK_LT)) != NULL)
      t = EX_LT;
    else if ((tok = match(TK_GT)) != NULL)
      t = EX_GT;
    else if ((tok = match(TK_LE)) != NULL)
      t = EX_LE;
    else if ((tok = match(TK_GE)) != NULL)
      t = EX_GE;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_shift();
    expr = new_expr_bop(t, &tyBool, tok, lhs, rhs);
  }
}

static Expr *parse_eq(void) {
  Expr *expr = parse_cmp();

  for (;;) {
    enum ExprKind t;
    Token *tok;
    if ((tok = match(TK_EQ)) != NULL)
      t = EX_EQ;
    else if ((tok = match(TK_NE)) != NULL)
      t = EX_NE;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_cmp();
    expr = new_expr_bop(t, &tyBool, tok, lhs, rhs);
  }
}

static Expr *parse_and(void) {
  Expr *expr = parse_eq();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_AND)) != NULL) {
      Expr *lhs = expr, *rhs = parse_eq();
      cast_integers(&lhs, &rhs, false);
      expr = new_expr_bop(EX_BITAND, lhs->type, tok, lhs, rhs);
    } else
      return expr;
  }
}

static Expr *parse_xor(void) {
  Expr *expr = parse_and();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_HAT)) != NULL) {
      Expr *lhs = expr, *rhs= parse_and();
      cast_integers(&lhs, &rhs, false);
      expr = new_expr_bop(EX_BITXOR, lhs->type, tok, lhs, rhs);
    } else
      return expr;
  }
}

static Expr *parse_or(void) {
  Expr *expr = parse_xor();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_OR)) != NULL) {
      Expr *lhs = expr, *rhs = parse_xor();
      cast_integers(&lhs, &rhs, false);
      expr = new_expr_bop(EX_BITOR, lhs->type, tok, lhs, rhs);
    } else
      return expr;
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

static Expr *parse_conditional(void) {
  Expr *expr = parse_logior();
  for (;;) {
    const Token *tok;
    if ((tok = match(TK_QUESTION)) == NULL)
      return expr;
    Expr *tval = parse_expr();
    consume(TK_COLON, "`:' expected");
    Expr *fval = parse_conditional();

    const Type *ttype = tval->type;
    const Type *ftype = fval->type;
    assert(ttype != NULL);
    assert(ftype != NULL);
    if (ttype->kind == TY_ARRAY) {
      ttype = array_to_ptr(ttype);
      tval = new_expr_cast(ttype, tval->token, tval);
    }
    if (ftype->kind == TY_ARRAY) {
      ftype = array_to_ptr(ftype);
      fval = new_expr_cast(ftype, fval->token, fval);
    }

    const Type *type = NULL;
    if (same_type(ttype, ftype)) {
      type = ttype;
    } else if (is_void_ptr(ttype) && ftype->kind == TY_PTR) {
      type = ftype;
    } else if (is_void_ptr(ftype) && ttype->kind == TY_PTR) {
      type = ttype;
    } else if (is_number(ttype->kind) && is_number(ftype->kind)) {
      if (ttype->num.kind > ftype->num.kind) {
        type = ttype;
        fval = new_expr_cast(ttype, fval->token, fval);
      } else {
        type = ftype;
        tval = new_expr_cast(ftype, tval->token, tval);
      }
    }

    if (type == NULL)
      parse_error(tok, "lhs and rhs must be same type");

    expr = new_expr_ternary(tok, expr, tval, fval, type);
  }
}

Expr *parse_assign(void) {
  static const struct {
    enum TokenKind tk;
    enum ExprKind ex;
    int mode;
  } kAssignWithOps[] = {
    { TK_ADD_ASSIGN, EX_ADD, 0 },
    { TK_SUB_ASSIGN, EX_SUB, 0 },
    { TK_MUL_ASSIGN, EX_MUL, 1 },
    { TK_DIV_ASSIGN, EX_DIV, 1 },
    { TK_MOD_ASSIGN, EX_MOD, 1 },
    { TK_AND_ASSIGN, EX_BITAND, 1 },
    { TK_OR_ASSIGN, EX_BITOR, 1 },
    { TK_HAT_ASSIGN, EX_BITXOR, 1 },
    { TK_LSHIFT_ASSIGN, EX_LSHIFT, 2 },
    { TK_RSHIFT_ASSIGN, EX_RSHIFT, 2 },
  };

  Expr *expr = parse_conditional();

  Token *tok = match(-1);
  if (tok != NULL) {
    if (tok->kind == TK_ASSIGN)
      return new_expr_bop(EX_ASSIGN, expr->type, tok, expr, parse_assign());

    for (int i = 0; i < (int)(sizeof(kAssignWithOps) / sizeof(*kAssignWithOps)); ++i) {
      if (tok->kind == kAssignWithOps[i].tk) {
        enum ExprKind kind = kAssignWithOps[i].ex;
        Expr *lhs = expr, *rhs = parse_assign();
        Expr *bop;
        switch (kAssignWithOps[i].mode) {
        case 0:  bop = new_expr_addsub(kind, tok, lhs, rhs, true); break;
        case 1:  bop = new_expr_muldiv(kind, tok, lhs, rhs, true); break;
        case 2:
          {
            const Type *ltype = lhs->type;
            const Type *rtype = rhs->type;
            assert(ltype != NULL);
            assert(rtype != NULL);
            if (!is_number(ltype->kind) || !is_number(rtype->kind))
              parse_error(tok, "Cannot use `%.*s' except numbers.", (int)(tok->end - tok->begin), tok->begin);
            bop = new_expr_bop(kind, lhs->type, tok, lhs, rhs);
          }
          break;
        default:  assert(false); bop = NULL; break;
        }
        assert(bop->type != NULL);
        return new_expr_unary(EX_ASSIGN_WITH, lhs->type, tok, bop);
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
