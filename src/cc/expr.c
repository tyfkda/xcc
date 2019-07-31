#include "expr.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "lexer.h"
#include "type.h"
#include "util.h"
#include "var.h"

static StructInfo *parse_struct(bool is_union);
static Expr *cast_expr(void);
static Expr *unary(void);

// Typedef

Map *typedef_map;

//

Expr *new_expr(enum ExprType type, const Type *valType, const Token *token) {
  Expr *expr = malloc(sizeof(*expr));
  expr->type = type;
  expr->valType = valType;
  expr->token = token;
  return expr;
}

bool is_const(Expr *expr) {
  // TODO: Handle constant variable.

  switch (expr->type) {
  case EX_NUM:
  case EX_STR:
    return true;
  default:
    return false;
  }
}

Expr *new_expr_numlit(const Type *type, const Token *token, const Num *num) {
  assert(type->type == TY_NUM);
  Expr *expr = new_expr(EX_NUM, type, token);
#if 0
  // TODO: Accept this
  expr->u.num = *num;
#else
  expr->u.num.ival = num->ival;
#endif
  return expr;
}

static Expr *new_expr_str(const Token *token, const char *str, size_t size) {
  Type *type = malloc(sizeof(*type));
  type->type = TY_ARRAY;
  type->u.pa.ptrof = &tyChar;
  type->u.pa.length = size;

  Expr *expr = new_expr(EX_STR, type, token);
  expr->u.str.buf = str;
  expr->u.str.size = size;
  return expr;
}

Expr *new_expr_varref(const char *name, const Type *type, bool global, const Token *token) {
  Expr *expr = new_expr(EX_VARREF, type, token);
  expr->u.varref.ident = name;
  expr->u.varref.global = global;
  return expr;
}

Expr *new_expr_bop(enum ExprType type, const Type *valType, const Token *token, Expr *lhs, Expr *rhs) {
  Expr *expr = new_expr(type, valType, token);
  expr->u.bop.lhs = lhs;
  expr->u.bop.rhs = rhs;
  return expr;
}

static Expr *new_expr_unary(enum ExprType type, const Type *valType, const Token *token, Expr *sub) {
  Expr *expr = new_expr(type, valType, token);
  expr->u.unary.sub = sub;
  return expr;
}

Expr *new_expr_deref(const Token *token, Expr *sub) {
  if (sub->valType->type != TY_PTR && sub->valType->type != TY_ARRAY)
    parse_error(token, "Cannot dereference raw type");
  return new_expr_unary(EX_DEREF, sub->valType->u.pa.ptrof, token, sub);
}

static Expr *new_expr_ternary(const Token *token, Expr *cond, Expr *tval, Expr *fval, const Type *type) {
  Expr *expr = new_expr(EX_TERNARY, type, token);
  expr->u.ternary.cond = cond;
  expr->u.ternary.tval = tval;
  expr->u.ternary.fval = fval;
  return expr;
}

Expr *new_expr_member(const Token *token, const Type *valType, Expr *target, const Token *acctok, const Token *ident, int index) {
  Expr *expr = new_expr(EX_MEMBER, valType, token);
  expr->u.member.target = target;
  expr->u.member.acctok = acctok;
  expr->u.member.ident = ident;
  expr->u.member.index = index;
  return expr;
}

static Expr *new_expr_funcall(const Token *token, Expr *func, Vector *args) {
  Expr *expr = new_expr(EX_FUNCALL, NULL, token);
  expr->u.funcall.func = func;
  expr->u.funcall.args = args;
  return expr;
}

static Expr *new_expr_comma(Vector *list) {
  Expr *expr = new_expr(EX_COMMA, NULL, NULL);
  expr->u.comma.list = list;
  return expr;
}

static Expr *funcall(Expr *func) {
  Vector *args = NULL;
  Token *token;
  if ((token = consume(TK_RPAR)) == NULL) {
    args = new_vector();
    for (;;) {
      Expr *arg = parse_assign();
      vec_push(args, arg);
      if ((token = consume(TK_RPAR)) != NULL)
        break;
      if (consume(TK_COMMA))
        continue;
      parse_error(NULL, "Comma or `)` expected");
    }
  }

  return new_expr_funcall(token, func, args);
}

Expr *array_index(const Token *token, Expr *array) {
  Expr *index = parse_expr();
  Token *tok;
  if ((tok = consume(TK_RBRACKET)) == NULL)
    parse_error(NULL, "`]' expected");
  //return new_expr_deref(add_expr(tok, array, index));
  return new_expr_unary(EX_DEREF, NULL, token, new_expr_bop(EX_ADD, NULL, token, array, index));
}

Expr *member_access(Expr *target, Token *acctok) {
  Token *ident;
  if (!(ident = consume(TK_IDENT)))
    parse_error(NULL, "`ident' expected");

  return new_expr_member(acctok, NULL, target, acctok, ident, -1);
}

static const Type *parse_enum(void) {
  Token *typeIdent = consume(TK_IDENT);

  if (consume(TK_LBRACE)) {
    if (!consume(TK_RBRACE)) {
      int value = 0;
      for (;;) {
        Token *numtok;
        Token *ident = numtok = consume(TK_IDENT);
        if (ident == NULL)
          parse_error(NULL, "ident expected");
        if (consume(TK_ASSIGN)) {
          numtok = fetch_token();
          Expr *expr = analyze_expr(parse_const(), false);
          if (!(is_const(expr) && is_number(expr->valType->type))) {
            parse_error(numtok, "const expected for enum");
          }
          value = expr->u.num.ival;
        }
        // Define
        (void)typeIdent;  // TODO: Define enum type with name.
        Initializer *init = malloc(sizeof(*init));
        init->type = vSingle;
        //init->u.single = new_expr_numlit(EX_INT, numtok, value);
        init->u.single = new_expr(EX_NUM, &tyEnum, numtok);
        init->u.single->u.num.ival = value;
        VarInfo *varinfo = define_global(&tyEnum, VF_CONST, ident, NULL);
        varinfo->u.g.init = init;
        ++value;

        if (consume(TK_COMMA))
          ;
        if (consume(TK_RBRACE))
          break;
      }
    }
  }
  return &tyEnum;
}

const Type *parse_raw_type(int *pflag) {
  const Type *type = NULL;

  int flag = 0;
  for (;;) {
    if (consume(TK_UNSIGNED)) {
      flag |= VF_UNSIGNED;
      continue;
    }
    if (consume(TK_KWCONST)) {
      flag |= VF_CONST;
      continue;
    }
    if (consume(TK_STATIC)) {
      flag |= VF_STATIC;
      continue;
    }
    if (consume(TK_EXTERN)) {
      flag |= VF_EXTERN;
      continue;
    }

    if (type != NULL)
      break;

    Token *structtok;
    Token *ident;
    if (((structtok = consume(TK_STRUCT)) != NULL) ||
        ((structtok = consume(TK_UNION)) != NULL)) {
      bool is_union = structtok->type == TK_UNION;
      const char *name = NULL;
      Token *ident;
      if ((ident = consume(TK_IDENT)) != NULL)
        name = ident->u.ident;

      StructInfo *sinfo = NULL;
      if (consume(TK_LBRACE)) {  // Definition
        sinfo = parse_struct(is_union);
        if (name != NULL) {
          StructInfo *exist = find_struct(name);
          if (exist != NULL)
            parse_error(ident, "`%s' already defined", name);
          define_struct(name, sinfo);
        }
      } else {
        if (name != NULL) {
          sinfo = (StructInfo*)map_get(struct_map, name);
          if (sinfo != NULL) {
            if (sinfo->is_union != is_union)
              parse_error(structtok, "Wrong tag for `%s'", name);
          }
        }
      }

      if (name == NULL && sinfo == NULL)
        parse_error(NULL, "Illegal struct/union usage");

      Type *stype = malloc(sizeof(*type));
      stype->type = TY_STRUCT;
      stype->u.struct_.name = name;
      stype->u.struct_.info = sinfo;
      type = stype;
    } else if (consume(TK_ENUM)) {
      type = parse_enum();
    } else if ((ident = consume(TK_IDENT)) != NULL) {
      type = map_get(typedef_map, ident->u.ident);
      if (type == NULL)
        unget_token(ident);
    } else {
      static const enum TokenType kKeywords[] = {
        TK_KWVOID, TK_KWCHAR, TK_KWSHORT, TK_KWINT, TK_KWLONG,
      };
      static const Type *kTypes[] = {
        &tyVoid, &tyChar, &tyShort, &tyInt, &tyLong,
      };
      const int N = sizeof(kTypes) / sizeof(*kTypes);
      for (int i = 0; i < N; ++i) {
        if (consume(kKeywords[i])) {
          type = kTypes[i];
          break;
        }
      }
    }
    if (type == NULL)
      break;
  }

  if (pflag != NULL)
    *pflag = flag;

  return type;
}

void not_void(const Type *type) {
  if (type->type == TY_VOID)
    parse_error(NULL, "`void' not allowed");
}

const Type *parse_type_modifier(const Type* type) {
  if (type == NULL)
    return NULL;

  for (;;) {
    if (consume(TK_KWCONST)) {
      // TODO: Reflect to the type.
      ;
    }
    if (consume(TK_MUL))
      type = ptrof(type);
    else
      break;
  }

  return type;
}

const Type *parse_type_suffix(const Type *type) {
  if (type == NULL)
    return NULL;

  if (!consume(TK_LBRACKET))
    return type;
  size_t length = -1;
  if (consume(TK_RBRACKET)) {
    // Arbitrary size.
  } else {
    const Token *tok = fetch_token();
    Expr *expr = analyze_expr(parse_const(), false);
    if (!(is_const(expr) && is_number(expr->valType->type)))
      parse_error(NULL, "syntax error");
    if (expr->u.num.ival <= 0)
      parse_error(tok, "Array size must be greater than 0, but %d", (int)expr->u.num.ival);
    length = expr->u.num.ival;
    if (!consume(TK_RBRACKET))
      parse_error(NULL, "`]' expected");
  }
  return arrayof(parse_type_suffix(type), length);
}

static Vector *parse_funparam_types(bool *pvaargs) {  // Vector<Type*>
  Vector *params = parse_funparams(pvaargs);
  Vector *param_types = NULL;
  if (params != NULL) {
    param_types = new_vector();
    for (int i = 0, len = params->len; i < len; ++i)
      vec_push(param_types, ((VarInfo*)params->data[i])->type);
  }
  return param_types;
}

bool parse_var_def(const Type **prawType, const Type** ptype, int *pflag, Token **pident) {
  const Type *rawType = prawType != NULL ? *prawType : NULL;
  if (rawType == NULL) {
    rawType = parse_raw_type(pflag);
    if (rawType == NULL)
      return false;
    if (prawType != NULL)
      *prawType = rawType;
  }

  const Type *type = parse_type_modifier(rawType);

  Token *ident = NULL;
  if (consume(TK_LPAR)) {  // Funcion type.
    consume(TK_MUL);  // Skip `*' if exists.
    ident = consume(TK_IDENT);
    //if (ident == NULL && !allow_noname)
    //  parse_error(NULL, "Ident expected");
    if (!consume(TK_RPAR))
      parse_error(NULL, "`)' expected");
    if (!consume(TK_LPAR))
      parse_error(NULL, "`(' expected");

    bool vaargs;
    Vector *param_types = parse_funparam_types(&vaargs);
    type = ptrof(new_func_type(type, param_types, vaargs));
  } else {
    if (type->type != TY_VOID) {
      ident = consume(TK_IDENT);
      //if (ident == NULL && !allow_noname)
      //  parse_error(NULL, "Ident expected");
    }
  }
  if (type->type != TY_VOID)
    type = parse_type_suffix(type);

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

Vector *parse_funparams(bool *pvaargs) {  // Vector<VarInfo*>, NULL=>old style.
  Vector *params = NULL;
  bool vaargs = false;
  if (consume(TK_RPAR)) {
    // Arbitrary funparams.
  } else {
    params = new_vector();
    for (;;) {
      if (consume(TK_DOTDOTDOT)) {
        vaargs = true;
        if (!consume(TK_RPAR))
          parse_error(NULL, "`)' expected");
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
        if (type->type == TY_VOID) {  // fun(void)
          if (!consume(TK_RPAR))
            parse_error(NULL, "`)' expected");
          break;
        }
      } else {
        not_void(type);
      }

      // If the type is array, handle it as a pointer.
      type = array_to_ptr(type);

      var_add(params, ident, type, flag);
      if (consume(TK_RPAR))
        break;
      if (consume(TK_COMMA))
        continue;
      parse_error(NULL, "Comma or `)' expected");
    }
  }
  *pvaargs = vaargs;
  return params;
}

// Parse struct or union definition `{...}`
static StructInfo *parse_struct(bool is_union) {
  Vector *members = new_vector();
  for (;;) {
    if (consume(TK_RBRACE))
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

      if (consume(TK_COMMA))
        continue;
      if (!consume(TK_SEMICOL))
        parse_error(NULL, "`;' expected");
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

static Expr *prim(void) {
  if (consume(TK_LPAR)) {
    Expr *expr = parse_expr();
    if (!consume(TK_RPAR))
      parse_error(NULL, "No close paren");
    return expr;
  }

  Token *tok;
  {
    const Type *type;
    if (((tok = consume(TK_CHARLIT)) != NULL && (type = &tyChar, true)) ||
        ((tok = consume(TK_INTLIT)) != NULL && (type = &tyInt, true)) ||
        ((tok = consume(TK_LONGLIT)) != NULL && (type = &tyLong, true))) {
      Num num = {tok->u.value};
      return new_expr_numlit(type, tok, &num);
    }
  }
  if ((tok = consume(TK_STR)))
    return new_expr_str(tok, tok->u.str.buf, tok->u.str.size);

  Token *ident;
  if ((ident = consume(TK_IDENT)) != NULL) {
    const char *name = ident->u.ident;
    return new_expr_varref(name, /*type*/NULL, /*global*/false, ident);
  }
  parse_error(NULL, "Number or Ident or open paren expected");
  return NULL;
}

static Expr *postfix(void) {
  Expr *expr = prim();

  for (;;) {
    Token *tok;
    if (consume(TK_LPAR))
      expr = funcall(expr);
    else if ((tok = consume(TK_LBRACKET)) != NULL)
      expr = array_index(tok, expr);
    else if ((tok = consume(TK_DOT)) != NULL)
      expr = member_access(expr, tok);
    else if ((tok = consume(TK_ARROW)) != NULL)
      expr = member_access(expr, tok);
    else if ((tok = consume(TK_INC)) != NULL)
      expr = new_expr_unary(EX_POSTINC, /*expr->valType*/NULL, tok, expr);
    else if ((tok = consume(TK_DEC)) != NULL)
      expr = new_expr_unary(EX_POSTDEC, /*expr->valType*/NULL, tok, expr);
    else
      return expr;
  }
}

static Expr *parse_sizeof(const Token *token) {
  const Type *type = NULL;
  Expr *expr = NULL;
  Token *tok;
  if ((tok = consume(TK_LPAR)) != NULL) {
    type = parse_full_type(NULL, NULL);
    if (type != NULL) {
      if (!consume(TK_RPAR))
        parse_error(NULL, "`)' expected");
    } else {
      unget_token(tok);
      expr = prim();
    }
  } else {
    expr = unary();
  }
  return new_expr_sizeof(token, type, expr);
}

static Expr *unary(void) {
  Token *tok;
  if ((tok = consume(TK_ADD)) != NULL) {
    Expr *expr = cast_expr();
    switch (expr->type) {
    case EX_NUM:
      return expr;
    default:
      return new_expr_unary(EX_POS, /*expr->valType*/NULL, tok, expr);
    }

    return expr;
  }

  if ((tok = consume(TK_SUB)) != NULL) {
    Expr *expr = cast_expr();
    switch (expr->type) {
    case EX_NUM:
      expr->u.num.ival = -expr->u.num.ival;
      return expr;
    default:
      return new_expr_unary(EX_NEG, /*expr->valType*/NULL, tok, expr);
    }
  }

  if ((tok = consume(TK_NOT)) != NULL) {
    Expr *expr = cast_expr();
    return new_expr_unary(EX_NOT, &tyBool, tok, expr);
  }

  if ((tok = consume(TK_AND)) != NULL) {
    Expr *expr = cast_expr();
    return new_expr_unary(EX_REF, /*ptrof(expr->valType)*/NULL, tok, expr);
  }

  if ((tok = consume(TK_MUL)) != NULL) {
    Expr *expr = cast_expr();
    return new_expr_unary(EX_DEREF, /*expr->valType->u.pa.ptrof*/NULL, tok, expr);
  }

  if ((tok = consume(TK_INC)) != NULL) {
    Expr *expr = unary();
    return new_expr_unary(EX_PREINC, /*expr->valType*/NULL, tok, expr);
  }

  if ((tok = consume(TK_DEC)) != NULL) {
    Expr *expr = unary();
    return new_expr_unary(EX_PREDEC, /*expr->valType*/NULL, tok, expr);
  }

  if ((tok = consume(TK_SIZEOF)) != NULL) {
    return parse_sizeof(tok);
  }

  return postfix();
}

static Expr *cast_expr(void) {
  Token *lpar;
  if ((lpar = consume(TK_LPAR)) != NULL) {
    int flag;
    const Token *token = fetch_token();
    const Type *type = parse_full_type(&flag, NULL);
    if (type != NULL) {  // Cast
      if (!consume(TK_RPAR))
        parse_error(NULL, "`)' expected");
      Expr *sub = cast_expr();
      Expr *expr = new_expr(EX_CAST, type, token);
      expr->u.cast.sub = sub;
      return expr;
    }
    unget_token(lpar);
  }
  return unary();
}

static Expr *mul(void) {
  Expr *expr = cast_expr();

  for (;;) {
    enum ExprType t;
    Token *tok;
    if ((tok = consume(TK_MUL)) != NULL)
      t = EX_MUL;
    else if ((tok = consume(TK_DIV)) != NULL)
      t = EX_DIV;
    else if ((tok = consume(TK_MOD)) != NULL)
      t = EX_MOD;
    else
      return expr;

    expr = new_expr_bop(t, NULL, tok, expr, cast_expr());
  }
}

static Expr *add(void) {
  Expr *expr = mul();

  for (;;) {
    enum ExprType t;
    Token *tok;
    if ((tok = consume(TK_ADD)) != NULL)
      t = EX_ADD;
    else if ((tok = consume(TK_SUB)) != NULL)
      t = EX_SUB;
    else
      return expr;

    expr = new_expr_bop(t, NULL, tok, expr, mul());
  }
}

static Expr *shift(void) {
  Expr *expr = add();

  for (;;) {
    enum ExprType t;
    Token *tok;
    if ((tok = consume(TK_LSHIFT)) != NULL)
      t = EX_LSHIFT;
    else if ((tok = consume(TK_RSHIFT)) != NULL)
      t = EX_RSHIFT;
    else
      return expr;

    Expr *lhs = expr, *rhs = add();
    expr = new_expr_bop(t, NULL, tok, lhs, rhs);
  }
}

static Expr *cmp(void) {
  Expr *expr = shift();

  for (;;) {
    enum ExprType t;
    Token *tok;
    if ((tok = consume(TK_LT)) != NULL)
      t = EX_LT;
    else if ((tok = consume(TK_GT)) != NULL)
      t = EX_GT;
    else if ((tok = consume(TK_LE)) != NULL)
      t = EX_LE;
    else if ((tok = consume(TK_GE)) != NULL)
      t = EX_GE;
    else
      return expr;

    Expr *lhs = expr, *rhs= shift();
    expr = new_expr_bop(t, &tyBool, tok, lhs, rhs);
  }
}

static Expr *eq(void) {
  Expr *expr = cmp();

  for (;;) {
    enum ExprType t;
    Token *tok;
    if ((tok = consume(TK_EQ)) != NULL)
      t = EX_EQ;
    else if ((tok = consume(TK_NE)) != NULL)
      t = EX_NE;
    else
      return expr;

    Expr *lhs = expr, *rhs= cmp();
    expr = new_expr_bop(t, &tyBool, tok, lhs, rhs);
  }
}

static Expr *and(void) {
  Expr *expr = eq();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_AND)) != NULL) {
      Expr *lhs = expr, *rhs= eq();
      expr = new_expr_bop(EX_BITAND, /*lhs->valType*/NULL, tok, lhs, rhs);
    } else
      return expr;
  }
}

static Expr *xor(void) {
  Expr *expr = and();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_HAT)) != NULL) {
      Expr *lhs = expr, *rhs= and();
      expr = new_expr_bop(EX_BITXOR, /*lhs->valType*/NULL, tok, lhs, rhs);
    } else
      return expr;
  }
}

static Expr *or(void) {
  Expr *expr = xor();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_OR)) != NULL) {
      Expr *lhs = expr, *rhs= xor();
      expr = new_expr_bop(EX_BITOR, /*lhs->valType*/NULL, tok, lhs, rhs);
    } else
      return expr;
  }
}

static Expr *logand(void) {
  Expr *expr = or();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_LOGAND)) != NULL)
      expr = new_expr_bop(EX_LOGAND, &tyBool, tok, expr, or());
    else
      return expr;
  }
}

static Expr *logior(void) {
  Expr *expr = logand();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_LOGIOR)) != NULL)
      expr = new_expr_bop(EX_LOGIOR, &tyBool, tok, expr, logand());
    else
      return expr;
  }
}

static Expr *conditional(void) {
  Expr *expr = logior();
  for (;;) {
    const Token *tok;
    if ((tok = consume(TK_QUESTION)) == NULL)
      return expr;
    Expr *t = parse_expr();
    if (!consume(TK_COLON))
      parse_error(NULL, "`:' expected");
    Expr *f = conditional();
    expr = new_expr_ternary(tok, expr, t, f, /*t->valType*/NULL);
  }
}

Expr *parse_assign(void) {
  Expr *expr = conditional();

  Token *tok;
  if ((tok = consume(TK_ASSIGN)) != NULL)
    return new_expr_bop(EX_ASSIGN, /*expr->valType*/NULL, tok, expr, parse_assign());
  enum ExprType t;
  if ((tok = consume(TK_ADD_ASSIGN)) != NULL)
    t = EX_ADD;
  else if ((tok = consume(TK_SUB_ASSIGN)) != NULL)
    t = EX_SUB;
  else if ((tok = consume(TK_MUL_ASSIGN)) != NULL)
    t = EX_MUL;
  else if ((tok = consume(TK_DIV_ASSIGN)) != NULL)
    t = EX_DIV;
  else if ((tok = consume(TK_MOD_ASSIGN)) != NULL)
    t = EX_MOD;
  else if ((tok = consume(TK_AND_ASSIGN)) != NULL)
    t = EX_BITAND;
  else if ((tok = consume(TK_OR_ASSIGN)) != NULL)
    t = EX_BITOR;
  else if ((tok = consume(TK_HAT_ASSIGN)) != NULL)
    t = EX_BITXOR;
  else if ((tok = consume(TK_LSHIFT_ASSIGN)) != NULL)
    t = EX_LSHIFT;
  else if ((tok = consume(TK_RSHIFT_ASSIGN)) != NULL)
    t = EX_RSHIFT;
  else
    return expr;

  return new_expr_unary(EX_ASSIGN_WITH, /*expr->valType*/NULL, tok,
                        new_expr_bop(t, NULL, tok, expr, parse_assign()));
}

Expr *parse_const(void) {
  return conditional();
}

Expr *parse_expr(void) {
  Expr *expr;
  Vector *list = NULL;
  for (;;) {
    expr = parse_assign();
    if (!consume(TK_COMMA))
      break;
    if (list == NULL)
      list = new_vector();
    vec_push(list, expr);
  }

  if (list == NULL)
    return expr;
  vec_push(list, expr);
  return new_expr_comma(list);
}
