#include "expr.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "lexer.h"
#include "util.h"

static const Type tyChar = {.type=TY_CHAR};
static const Type tyShort = {.type=TY_SHORT};
static const Type tyInt = {.type=TY_INT};
static const Type tyLong = {.type=TY_LONG};
static const Type tyEnum = {.type=TY_ENUM};
#define tyBool  tyInt
#define tySize  tyLong

static StructInfo *parse_struct(bool is_union);
static Expr *cast_expr(void);
static Expr *unary(void);

//

int var_find(Vector *lvars, const char *name) {
  for (int i = 0, len = lvars->len; i < len; ++i) {
    VarInfo *info = (VarInfo*)lvars->data[i];
    if (info->name != NULL && strcmp(info->name, name) == 0)
      return i;
  }
  return -1;
}

void var_add(Vector *lvars, const Token *ident, const Type *type, int flag, Initializer *init) {
  // init is only for static local variable.
  const char *name = NULL;
  const char *label = NULL;
  assert(init == NULL || flag & VF_STATIC);
  if (ident != NULL) {
    name = ident->u.ident;
    int idx = var_find(lvars, name);
    if (idx >= 0)
      parse_error(ident, "`%s' already defined", name);
    if (flag & VF_STATIC) {  // TODO: Handle static specifier in local definition.
      label = alloc_label();
      define_global(type, flag, alloc_ident(label, NULL, NULL), init);
    }
  }

  VarInfo *info = malloc(sizeof(*info));
  info->name = name;
  info->type = type;
  info->flag = flag;
  info->u.l.label = label;
  info->offset = -1;
  vec_push(lvars, info);
}

// Struct

Map *struct_map;

// Typedef

Map *typedef_map;

// Global

Map *gvar_map;

VarInfo *find_global(const char *name) {
  return (VarInfo*)map_get(gvar_map, name);
}

void define_global(const Type *type, int flag, const Token *ident, Initializer *init) {
  const char *name = ident->u.ident;
  VarInfo *varinfo = find_global(name);
  if (varinfo != NULL && !(varinfo->flag & VF_EXTERN))
    parse_error(ident, "`%s' already defined", name);
  varinfo = malloc(sizeof(*varinfo));
  varinfo->name = name;
  varinfo->type = type;
  varinfo->flag = flag;
  varinfo->u.g.init = init;
  varinfo->offset = 0;
  map_put(gvar_map, name, varinfo);
}

// Type

// Call before accessing struct member to ensure that struct is declared.
void ensure_struct(Type *type, const Token *token) {
  assert(type->type == TY_STRUCT || type->type == TY_UNION);
  if (type->u.struct_.info == NULL) {
    // TODO: Search from name.
    StructInfo *sinfo = (StructInfo*)map_get(struct_map, type->u.struct_.name);
    if (sinfo == NULL)
      parse_error(token, "Accessing unknown struct(%s)'s member", type->u.struct_.name);
    type->u.struct_.info = sinfo;
  }
}

void dump_type(FILE *fp, const Type *type) {
  switch (type->type) {
  case TY_VOID: fprintf(fp, "void"); break;
  case TY_CHAR: fprintf(fp, "char"); break;
  case TY_INT: fprintf(fp, "int"); break;
  case TY_LONG: fprintf(fp, "long"); break;
  case TY_PTR: dump_type(fp, type->u.pa.ptrof); fprintf(fp, "*"); break;
  case TY_ARRAY: dump_type(fp, type->u.pa.ptrof); fprintf(fp, "[%d]", (int)type->u.pa.length); break;
  default: assert(false);
  }
}

static bool is_number(enum eType type) {
  switch (type) {
  case TY_CHAR:
  case TY_SHORT:
  case TY_INT:
  case TY_LONG:
  case TY_ENUM:
    return true;
  default:
    return false;
  }
}

bool is_struct_or_union(enum eType type) {
  switch (type) {
  case TY_STRUCT:
  case TY_UNION:
    return true;
  default:
    return false;
  }
}

bool same_type(const Type *type1, const Type *type2) {
  for (;;) {
    if (type1->type != type2->type)
      return false;

    switch (type1->type) {
    case TY_VOID:
    case TY_CHAR:
    case TY_SHORT:
    case TY_INT:
    case TY_LONG:
      return true;
    case TY_ENUM:
      return true;
    case TY_ARRAY:
    case TY_PTR:
      type1 = type1->u.pa.ptrof;
      type2 = type2->u.pa.ptrof;
      continue;
    case TY_FUNC:
      if (!same_type(type1->u.func.ret, type2->u.func.ret) ||
          type1->u.func.params->len != type2->u.func.params->len)
        return false;
      for (int i = 0, len = type1->u.func.params->len; i < len; ++i) {
        VarInfo *v1 = (VarInfo*)type1->u.func.params->data[i];
        VarInfo *v2 = (VarInfo*)type2->u.func.params->data[i];
        if (!same_type(v1->type, v2->type))
          return false;
      }
      return true;
    case TY_STRUCT:
    case TY_UNION:
      {
        if (type1->u.struct_.info != NULL) {
          if (type2->u.struct_.info != NULL)
            return type1->u.struct_.info == type2->u.struct_.info;
          const Type *tmp = type1;
          type1 = type2;
          type2 = tmp;
        } else if (type2->u.struct_.info == NULL) {
          return strcmp(type1->u.struct_.name, type2->u.struct_.name) == 0;
        }
        // Find type1 from name.
        StructInfo *sinfo = (StructInfo*)map_get(struct_map, type1->u.struct_.name);
        if (sinfo == NULL)
          return false;
        return sinfo == type2->u.struct_.info;
      }
    }
  }
}

static Type* ptrof(const Type *type) {
  Type *ptr = malloc(sizeof(*ptr));
  ptr->type = TY_PTR;
  ptr->u.pa.ptrof = type;
  return ptr;
}

static const Type *array_to_ptr(const Type *type) {
  if (type->type != TY_ARRAY)
    return type;
  return ptrof(type->u.pa.ptrof);
}

Type* arrayof(const Type *type, size_t length) {
  Type *arr = malloc(sizeof(*arr));
  arr->type = TY_ARRAY;
  arr->u.pa.ptrof = type;
  arr->u.pa.length = length;
  return arr;
}

Type* new_func_type(const Type *ret, const Vector *params, bool vaargs) {
  Type *f = malloc(sizeof(*f));
  f->type = TY_FUNC;
  f->u.func.ret = ret;
  f->u.func.vaargs = vaargs;

  Vector *newparams = NULL;
  if (params != NULL) {
    // Clone params.
    newparams = new_vector();
    for (int i = 0; i < params->len; ++i)
      vec_push(newparams, params->data[i]);
  }
  f->u.func.params = newparams;
  return f;
}

// Scope

Scope *new_scope(Scope *parent, Vector *vars) {
  Scope *scope = malloc(sizeof(*scope));
  scope->parent = parent;
  scope->vars = vars;
  return scope;
}

VarInfo *scope_find(Scope *scope, const char *name) {
  for (;; scope = scope->parent) {
    if (scope == NULL)
      return NULL;
    if (scope->vars != NULL) {
      int idx = var_find(scope->vars, name);
      if (idx >= 0)
        return (VarInfo*)scope->vars->data[idx];
    }
  }
}

// Scope

static Scope *curscope;

Scope *enter_scope(Defun *defun, Vector *vars) {
  Scope *scope = new_scope(curscope, vars);
  curscope = scope;
  vec_push(defun->all_scopes, scope);
  return scope;
}

void exit_scope(void) {
  assert(curscope != NULL);
  curscope = curscope->parent;
}

void add_cur_scope(const Token *ident, const Type *type, int flag, Initializer *init) {
  if (curscope->vars == NULL)
    curscope->vars = new_vector();
  var_add(curscope->vars, ident, type, flag, init);
}

//

Expr *new_expr(enum ExprType type, const Type *valType) {
  Expr *expr = malloc(sizeof(*expr));
  expr->type = type;
  expr->valType = valType;
  return expr;
}

bool can_cast(const Type *dst, const Type *src, Expr *src_expr, bool is_explicit) {
  if (same_type(dst, src))
    return true;

  if (dst->type == TY_VOID || src->type == TY_VOID)
    return false;

  switch (dst->type) {
  case TY_CHAR:
    switch (src->type) {
    case TY_SHORT:
    case TY_INT:
    case TY_LONG:
      return true;  // TODO: Raise warning if implicit.
    default:  break;
    }
    break;
  case TY_SHORT:
    switch (src->type) {
    case TY_CHAR:
    case TY_INT:
    case TY_LONG:
      return true;  // TODO: Raise warning if implicit.
    default:  break;
    }
    break;
  case TY_INT:
    switch (src->type) {
    case TY_CHAR:
    case TY_SHORT:
    case TY_LONG:
    case TY_ENUM:
      return true;
    default:  break;
    }
    break;
  case TY_LONG:
    switch (src->type) {
    case TY_CHAR:
    case TY_SHORT:
    case TY_INT:
      return true;
    case TY_PTR:
      if (is_explicit) {
        // TODO: Check sizeof(long) is same as sizeof(ptr)
        return true;
      }
      break;
    default:  break;
    }
    break;
  case TY_ENUM:
    switch (src->type) {
    case TY_INT:
      return true;
    case TY_CHAR:
    case TY_LONG:
      if (is_explicit)
        return true;
      break;
    case TY_PTR:
      if (is_explicit) {
        // TODO: Check sizeof(long) is same as sizeof(ptr)
        return true;
      }
      break;
    default:  break;
    }
    break;
  case TY_PTR:
    switch (src->type) {
    case TY_INT:
      if (src_expr->type == EX_INT && src_expr->u.value == 0)  // Special handling for 0 to pointer.
        return true;
      if (is_explicit)
        return true;
      break;
    case TY_LONG:
      if (src_expr->type == EX_LONG && src_expr->u.value == 0)  // Special handling for 0 to pointer.
        return true;
      if (is_explicit)
        return true;
      break;
    case TY_PTR:
      if (is_explicit)
        return true;
      // void* is interchangable with any pointer type.
      if (dst->u.pa.ptrof->type == TY_VOID || src->u.pa.ptrof->type == TY_VOID)
        return true;
      break;
    case TY_ARRAY:
      if (is_explicit)
        return true;
      if (same_type(dst->u.pa.ptrof, src->u.pa.ptrof) ||
          can_cast(dst, ptrof(src->u.pa.ptrof), src_expr, is_explicit))
        return true;
      break;
    default:  break;
    }
    break;
  case TY_ARRAY:
    switch (src->type) {
    case TY_PTR:
      if (is_explicit && same_type(dst->u.pa.ptrof, src->u.pa.ptrof))
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

Expr *new_expr_numlit(enum ExprType exprtype, intptr_t val) {
  const Type *type = NULL;
  switch (exprtype) {
  case EX_CHAR:  type = &tyChar; break;
  case EX_SHORT: type = &tyShort; break;
  case EX_INT:   type = &tyInt; break;
  case EX_LONG:  type = &tyLong; break;
  default: assert(false); break;
  }
  Expr *expr = new_expr(exprtype, type);
  expr->u.value = val;
  return expr;
}

static Expr *new_expr_str(const char *str, size_t size) {
  Type *type = malloc(sizeof(*type));
  type->type = TY_ARRAY;
  type->u.pa.ptrof = &tyChar;
  type->u.pa.length = size;

  Expr *expr = new_expr(EX_STR, type);
  expr->u.str.buf = str;
  expr->u.str.size = size;
  return expr;
}

Expr *new_expr_varref(const char *name, const Type *type, bool global) {
  Expr *expr = new_expr(EX_VARREF, type);
  expr->u.varref.ident = name;
  expr->u.varref.global = global;
  return expr;
}

Expr *new_expr_bop(enum ExprType type, const Type *valType, Expr *lhs, Expr *rhs) {
  Expr *expr = new_expr(type, valType);
  expr->u.bop.lhs = lhs;
  expr->u.bop.rhs = rhs;
  return expr;
}

static Expr *new_expr_unary(enum ExprType type, const Type *valType, Expr *sub) {
  Expr *expr = new_expr(type, valType);
  expr->u.unary.sub = sub;
  return expr;
}

Expr *new_expr_deref(Expr *sub) {
  if (sub->valType->type != TY_PTR && sub->valType->type != TY_ARRAY)
    parse_error(NULL, "Cannot dereference raw type");
  return new_expr_unary(EX_DEREF, sub->valType->u.pa.ptrof, sub);
}

Expr *new_expr_cast(const Type *type, Expr *sub, bool is_explicit) {
  if (type->type == TY_VOID || sub->valType->type == TY_VOID)
    parse_error(NULL, "cannot use `void' as a value");

  if (!can_cast(type, sub->valType, sub, is_explicit))
    parse_error(NULL, "Cannot convert value from type %d to %d", sub->valType->type, type->type);

  if (same_type(type, sub->valType))
    return sub;

  Expr *expr = new_expr(EX_CAST, type);
  expr->u.cast.sub = sub;
  return expr;
}

static Expr *new_expr_ternary(Expr *cond, Expr *tval, Expr *fval, const Type *type) {
  Expr *expr = new_expr(EX_TERNARY, type);
  expr->u.ternary.cond = cond;
  expr->u.ternary.tval = tval;
  expr->u.ternary.fval = fval;
  return expr;
}

Expr *new_expr_member(const Type *valType, Expr *target, const Token *acctok, const Token *ident, int index) {
  Expr *expr = new_expr(EX_MEMBER, valType);
  expr->u.member.target = target;
  expr->u.member.acctok = acctok;
  expr->u.member.ident = ident;
  expr->u.member.index = index;
  return expr;
}

static Expr *new_expr_sizeof(const Type *type, Expr *sub) {
  Expr *expr = new_expr(EX_SIZEOF, &tySize);
  expr->u.sizeof_.type = type;
  expr->u.sizeof_.sub = sub;
  return expr;
}

static Expr *new_expr_funcall(Expr *func, Vector *args) {
  Expr *expr = new_expr(EX_FUNCALL, NULL);
  expr->u.funcall.func = func;
  expr->u.funcall.args = args;
  return expr;
}

static Expr *new_expr_comma(Vector *list) {
  Expr *expr = new_expr(EX_COMMA, NULL);
  expr->u.comma.list = list;
  return expr;
}

static Expr *funcall(Expr *func) {
  Vector *args = NULL;
  Token *tok;
  if ((tok = consume(TK_RPAR)) == NULL) {
    args = new_vector();
    for (;;) {
      Expr *arg = parse_assign();
      vec_push(args, arg);
      if ((tok = consume(TK_RPAR)) != NULL)
        break;
      if (consume(TK_COMMA))
        continue;
      parse_error(NULL, "Comma or `)` expected");
    }
  }

  return new_expr_funcall(func, args);
}

// pointer +|- num
static Expr *add_ptr_num(enum ExprType type, Expr *ptr, Expr *num) {
  const Type *ptr_type = ptr->valType;
  if (ptr_type->type == TY_ARRAY)
    ptr_type = array_to_ptr(ptr_type);
  return new_expr_bop(type, ptr_type, ptr,
                      new_expr_bop(EX_MUL, &tySize,
                                   new_expr_cast(&tySize, num, false),
                                   new_expr_sizeof(ptr_type->u.pa.ptrof, NULL)));
}

static Expr *diff_ptr(const Token *tok, Expr *lhs, Expr *rhs) {
  if (!same_type(lhs->valType, rhs->valType))
    parse_error(tok, "Different pointer diff");
  const Type *elem_type = lhs->valType;
  if (elem_type->type == TY_PTR)
    elem_type = elem_type->u.pa.ptrof;
  return new_expr_bop(EX_DIV, &tySize,
                      new_expr_bop(EX_SUB, &tySize, lhs, rhs),
                      new_expr_sizeof(elem_type, NULL));
}

Expr *add_expr(Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  if (is_number(lhs->valType->type) && same_type(lhs->valType, rhs->valType))
    return new_expr_bop(EX_ADD, lhs->valType, lhs, rhs);

  switch (lhs->valType->type) {
  case TY_CHAR:
    switch (rhs->valType->type) {
    case TY_SHORT: case TY_INT: case TY_LONG:
      if (!keep_left)
        return new_expr_bop(EX_ADD, rhs->valType, new_expr_cast(rhs->valType, lhs, false), rhs);
      return new_expr_bop(EX_ADD, lhs->valType, lhs, new_expr_cast(lhs->valType, rhs, false));
    default:
      break;
    }
    break;

  case TY_SHORT:
    switch (rhs->valType->type) {
    case TY_INT: case TY_LONG:
      if (!keep_left)
        return new_expr_bop(EX_ADD, rhs->valType, new_expr_cast(rhs->valType, lhs, false), rhs);
      // Fallthrough
    case TY_CHAR:
      return new_expr_bop(EX_ADD, lhs->valType, lhs, new_expr_cast(lhs->valType, rhs, false));
    case TY_PTR: case TY_ARRAY:
      if (!keep_left)
        return add_ptr_num(EX_ADD, rhs, lhs);
      break;
    default:
      break;
    }
    break;

  case TY_INT:
    switch (rhs->valType->type) {
    case TY_LONG:
      if (!keep_left)
        return new_expr_bop(EX_ADD, rhs->valType, new_expr_cast(rhs->valType, lhs, false), rhs);
      // Fallthrough
    case TY_CHAR: case TY_SHORT:
      return new_expr_bop(EX_ADD, lhs->valType, lhs, new_expr_cast(lhs->valType, rhs, false));
    case TY_PTR: case TY_ARRAY:
      if (!keep_left)
        return add_ptr_num(EX_ADD, rhs, lhs);
      break;
    default:
      break;
    }
    break;

  case TY_LONG:
    switch (rhs->valType->type) {
    case TY_CHAR: case TY_SHORT: case TY_INT:
      return new_expr_bop(EX_ADD, lhs->valType, lhs, new_expr_cast(lhs->valType, rhs, false));
    case TY_PTR: case TY_ARRAY:
      if (!keep_left)
        return add_ptr_num(EX_ADD, rhs, lhs);
      break;
    default:
      break;
    }
    break;

  case TY_PTR: case TY_ARRAY:
    switch (rhs->valType->type) {
    case TY_CHAR: case TY_SHORT: case TY_INT: case TY_LONG:
      return add_ptr_num(EX_ADD, lhs, rhs);
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

static Expr *sub_expr(Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  if (is_number(lhs->valType->type) && same_type(lhs->valType, rhs->valType))
    return new_expr_bop(EX_SUB, lhs->valType, lhs, rhs);

  switch (lhs->valType->type) {
  case TY_CHAR:
    switch (rhs->valType->type) {
    case TY_SHORT: case TY_INT: case TY_LONG:
      if (!keep_left)
        return new_expr_bop(EX_SUB, rhs->valType, new_expr_cast(rhs->valType, lhs, false), rhs);
      return new_expr_bop(EX_SUB, lhs->valType, lhs, new_expr_cast(lhs->valType, rhs, false));
    default:
      break;
    }
    break;

  case TY_SHORT:
    switch (rhs->valType->type) {
    case TY_INT: case TY_LONG:
      if (!keep_left)
        return new_expr_bop(EX_SUB, rhs->valType, new_expr_cast(rhs->valType, lhs, false), rhs);
      // Fallthrough
    case TY_CHAR:
      return new_expr_bop(EX_SUB, lhs->valType, lhs, new_expr_cast(lhs->valType, rhs, false));
    default:
      break;
    }
    break;

  case TY_INT:
    switch (rhs->valType->type) {
    case TY_LONG:
      if (!keep_left)
        return new_expr_bop(EX_SUB, rhs->valType, new_expr_cast(rhs->valType, lhs, false), rhs);
      // Fallthrough
    case TY_CHAR: case TY_SHORT:
      return new_expr_bop(EX_SUB, lhs->valType, lhs, new_expr_cast(lhs->valType, rhs, false));
    default:
      break;
    }
    break;

  case TY_LONG:
    switch (rhs->valType->type) {
    case TY_CHAR: case TY_SHORT: case TY_INT:
      return new_expr_bop(EX_SUB, lhs->valType, lhs, new_expr_cast(lhs->valType, rhs, false));
    default:
      break;
    }
    break;

  case TY_PTR:
    switch (rhs->valType->type) {
    case TY_CHAR: case TY_INT: case TY_SHORT: case TY_LONG:
      return add_ptr_num(EX_SUB, lhs, rhs);
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

Expr *array_index(Expr *array) {
  Expr *index = parse_expr();
  Token *tok;
  if ((tok = consume(TK_RBRACKET)) == NULL)
    parse_error(NULL, "`]' expected");
  //return new_expr_deref(add_expr(tok, array, index));
  return new_expr_unary(EX_DEREF, NULL, new_expr_bop(EX_ADD, NULL, array, index));
}

bool member_access_recur(const Type *type, const Token *ident, Vector *stack) {
  assert(type->type == TY_STRUCT || type->type == TY_UNION);
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
    } else if (info->type->type == TY_STRUCT || info->type->type == TY_UNION) {
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

Expr *member_access(Expr *target, Token *acctok) {
  Token *ident;
  if (!(ident = consume(TK_IDENT)))
    parse_error(NULL, "`ident' expected");

  return new_expr_member(NULL, target, acctok, ident, -1);
}

static const Type *parse_enum(void) {
  Token *typeIdent = consume(TK_IDENT);

  if (consume(TK_LBRACE)) {
    if (!consume(TK_RBRACE)) {
      int value = 0;
      for (;;) {
        Token *ident = consume(TK_IDENT);
        if (ident == NULL)
          parse_error(NULL, "ident expected");
        if (consume(TK_ASSIGN)) {
          Token *tok = fetch_token();
          Expr *expr = parse_const();
          switch (expr->type) {  // TODO: Accept constexpr.
          case  EX_CHAR:
          case  EX_SHORT:
          case  EX_INT:
          case  EX_LONG:
            value = expr->u.value;
            break;
          default:
            parse_error(tok, "const expected for enum");
            break;
          }
        }
        // Define
        (void)typeIdent;  // TODO: Define enum type with name.
        Initializer *init = malloc(sizeof(*init));
        init->type = vSingle;
        init->u.single = new_expr_numlit(EX_INT, value);
        define_global(&tyEnum, VF_CONST, ident, init);
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
  Type *type = NULL;
  Token *structtok;
  Token *ident;

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
    break;
  }
  if (pflag != NULL)
    *pflag = flag;

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
        StructInfo *exist = (StructInfo*)map_get(struct_map, name);
        if (exist != NULL)
          parse_error(ident, "`%s' already defined", name);
        map_put(struct_map, name, sinfo);
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

    type = malloc(sizeof(*type));
    type->type = (structtok->type == TK_STRUCT) ? TY_STRUCT : TY_UNION;
    type->u.struct_.name = name;
    type->u.struct_.info = sinfo;
  } else if (consume(TK_ENUM)) {
    return parse_enum();
  } else if ((ident = consume(TK_IDENT)) != NULL) {
    type = map_get(typedef_map, ident->u.ident);
    if (type == NULL)
      unget_token(ident);
  } else {
    static const enum TokenType kKeywords[] = {
      TK_KWVOID, TK_KWCHAR, TK_KWSHORT, TK_KWINT, TK_KWLONG,
    };
    static const enum eType kTypes[] = {
      TY_VOID, TY_CHAR, TY_SHORT, TY_INT, TY_LONG,
    };
    const int N = sizeof(kTypes) / sizeof(*kTypes);
    for (int i = 0; i < N; ++i) {
      if (consume(kKeywords[i])) {
        type = malloc(sizeof(*type));
        type->type = kTypes[i];
        break;
      }
    }
  }
  return type;
}

void not_void(const Type *type) {
  if (type->type == TY_VOID)
    parse_error(NULL, "`void' not allowed");
}

const Type *parse_type_modifier(const Type* type) {
  if (type == NULL)
    return NULL;

  while (consume(TK_MUL))
    type = ptrof(type);

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
    Expr *expr = parse_const();
    if (expr->type != EX_INT)  // TODO: Constant expression.
      parse_error(NULL, "syntax error");
    if (expr->u.value <= 0)
      parse_error(/*tok*/NULL, "Array size must be greater than 0, but %d", (int)expr->u.value);
    length = expr->u.value;
    if (!consume(TK_RBRACKET))
      parse_error(NULL, "`]' expected");
  }
  return arrayof(parse_type_suffix(type), length);
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
    Vector *params = funparams(&vaargs);
    type = ptrof(new_func_type(type, params, vaargs));
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

Vector *funparams(bool *pvaargs) {
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

      var_add(params, ident, type, flag, NULL);
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

    const Type *type;
    int flag;
    Token *ident;
    if (!parse_var_def(NULL, &type, &flag, &ident))
      parse_error(NULL, "type expected");
    not_void(type);

    if (!consume(TK_SEMICOL))
      parse_error(NULL, "`;' expected");
    var_add(members, ident, type, flag, NULL);
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
    enum ExprType nt;
    if (((tok = consume(TK_CHARLIT)) != NULL && (nt = EX_CHAR, true)) ||
        ((tok = consume(TK_INTLIT)) != NULL && (nt = EX_INT, true)) ||
        ((tok = consume(TK_LONGLIT)) != NULL && (nt = EX_LONG, true)))
      return new_expr_numlit(nt, tok->u.value);
  }
  if ((tok = consume(TK_STR)))
    return new_expr_str(tok->u.str.buf, tok->u.str.size);

  Token *ident;
  if ((ident = consume(TK_IDENT)) != NULL) {
    const char *name = ident->u.ident;
    return new_expr_varref(name, /*type*/NULL, /*global*/false);
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
    else if (consume(TK_LBRACKET))
      expr = array_index(expr);
    else if ((tok = consume(TK_DOT)) != NULL)
      expr = member_access(expr, tok);
    else if ((tok = consume(TK_ARROW)) != NULL)
      expr = member_access(expr, tok);
    else if (consume(TK_INC))
      expr = new_expr_unary(EX_POSTINC, /*expr->valType*/NULL, expr);
    else if (consume(TK_DEC))
      expr = new_expr_unary(EX_POSTDEC, /*expr->valType*/NULL, expr);
    else
      return expr;
  }
}

static Expr *parse_sizeof(void) {
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
  return new_expr_sizeof(type, expr);
}

static Expr *unary(void) {
  Token *tok;
  if ((tok = consume(TK_ADD)) != NULL) {
    Expr *expr = cast_expr();
    switch (expr->type) {
    case EX_CHAR:
    case EX_SHORT:
    case EX_INT:
    case EX_LONG:
      expr->u.value = -expr->u.value;
      return expr;
    default:
      return new_expr_unary(EX_POS, /*expr->valType*/NULL, expr);
    }

    return expr;
  }

  if ((tok = consume(TK_SUB)) != NULL) {
    Expr *expr = cast_expr();
    switch (expr->type) {
    case EX_CHAR:
    case EX_SHORT:
    case EX_INT:
    case EX_LONG:
      expr->u.value = -expr->u.value;
      return expr;
    default:
      return new_expr_unary(EX_NEG, /*expr->valType*/NULL, expr);
    }
  }

  if ((tok = consume(TK_NOT)) != NULL) {
    Expr *expr = cast_expr();
    return new_expr_unary(EX_NOT, &tyBool, expr);
  }

  if (consume(TK_AND)) {
    Expr *expr = cast_expr();
    return new_expr_unary(EX_REF, /*ptrof(expr->valType)*/NULL, expr);
  }

  if (consume(TK_MUL)) {
    Expr *expr = cast_expr();
    return new_expr_unary(EX_DEREF, /*expr->valType->u.pa.ptrof*/NULL, expr);
  }

  if (consume(TK_INC)) {
    Expr *expr = unary();
    return new_expr_unary(EX_PREINC, /*expr->valType*/NULL, expr);
  }

  if (consume(TK_DEC)) {
    Expr *expr = unary();
    return new_expr_unary(EX_PREDEC, /*expr->valType*/NULL, expr);
  }

  if (consume(TK_SIZEOF)) {
    return parse_sizeof();
  }

  return postfix();
}

static Expr *cast_expr(void) {
  Token *lpar;
  if ((lpar = consume(TK_LPAR)) != NULL) {
    int flag;
    const Type *type = parse_full_type(&flag, NULL);
    if (type != NULL) {  // Cast
      if (!consume(TK_RPAR))
        parse_error(NULL, "`)' expected");
      Expr *sub = cast_expr();
      Expr *expr = new_expr(EX_CAST, type);
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

    expr = new_expr_bop(t, NULL, expr, cast_expr());
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

    expr = new_expr_bop(t, NULL, expr, mul());
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
    expr = new_expr_bop(t, NULL, lhs, rhs);
  }
}

static bool cast_numbers(Expr **pLhs, Expr **pRhs, bool keep_left) {
  enum eType ltype = (*pLhs)->valType->type, rtype = (*pRhs)->valType->type;
  if (!is_number(ltype) || !is_number(rtype))
    return false;

  if (ltype == TY_ENUM)
    ltype = TY_INT;
  if (rtype == TY_ENUM)
    rtype = TY_INT;

  if (ltype > rtype || keep_left)
    *pRhs = new_expr_cast((*pLhs)->valType, *pRhs, false);
  else if (ltype < rtype)
    *pLhs = new_expr_cast((*pRhs)->valType, *pLhs, false);
  return true;
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
    expr = new_expr_bop(t, &tyBool, lhs, rhs);
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
    expr = new_expr_bop(t, &tyBool, lhs, rhs);
  }
}

static Expr *and(void) {
  Expr *expr = eq();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_AND)) != NULL) {
      Expr *lhs = expr, *rhs= eq();
      expr = new_expr_bop(EX_BITAND, /*lhs->valType*/NULL, lhs, rhs);
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
      expr = new_expr_bop(EX_BITXOR, /*lhs->valType*/NULL, lhs, rhs);
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
      expr = new_expr_bop(EX_BITOR, /*lhs->valType*/NULL, lhs, rhs);
    } else
      return expr;
  }
}

static Expr *logand(void) {
  Expr *expr = or();
  for (;;) {
    if (consume(TK_LOGAND))
      expr = new_expr_bop(EX_LOGAND, &tyBool, expr, or());
    else
      return expr;
  }
}

static Expr *logior(void) {
  Expr *expr = logand();
  for (;;) {
    if (consume(TK_LOGIOR))
      expr = new_expr_bop(EX_LOGIOR, &tyBool, expr, logand());
    else
      return expr;
  }
}

static Expr *conditional(void) {
  Expr *expr = logior();
  for (;;) {
    if (!consume(TK_QUESTION))
      return expr;
    Expr *t = parse_expr();
    if (!consume(TK_COLON))
      parse_error(NULL, "`:' expected");
    Expr *f = conditional();
    expr = new_expr_ternary(expr, t, f, /*t->valType*/NULL);
  }
}

Expr *parse_assign(void) {
  Expr *expr = conditional();

  if (consume(TK_ASSIGN))
    return new_expr_bop(EX_ASSIGN, /*expr->valType*/NULL, expr, parse_assign());
  enum ExprType t;
  Token *tok;
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
  else
    return expr;

  return new_expr_unary(EX_ASSIGN_WITH, /*expr->valType*/NULL,
                        new_expr_bop(t, NULL, expr, parse_assign()));
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

//

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
      parse_error(/*tok*/NULL, "Cannot compare pointer to other types");
    if (rt->type != TY_PTR) {
      if (lt == lhs->valType)
        expr->u.bop.rhs = new_expr_cast(lhs->valType, rhs, false);
      else
        expr->u.bop.lhs = new_expr_cast(rhs->valType, lhs, false);
    }
  } else {
    if (!cast_numbers(&expr->u.bop.lhs, &expr->u.bop.rhs, false))
      parse_error(/*tok*/NULL, "Cannot compare except numbers");
  }
}

// Traverse expr to check semantics and determine value type.
Expr *analyze_expr(Expr *expr, bool keep_left) {
  switch (expr->type) {
  // Literals
  case EX_CHAR:
  case EX_SHORT:
  case EX_INT:
  case EX_LONG:
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
        if (varinfo != NULL)
          type = varinfo->type;
      }
      if (type == NULL) {
        VarInfo *varinfo = find_global(name);
        if (varinfo != NULL) {
          global = true;
          type = varinfo->type;
          if (type->type == TY_ENUM) {
            // Enum value is embeded directly.
            assert(varinfo->u.g.init->type == vSingle);
            return varinfo->u.g.init->u.single;
          }
        }
      }
      if (type == NULL)
        parse_error(/*ident*/NULL, "Undefined `%s'", name);
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
      return add_expr(NULL, expr->u.bop.lhs, expr->u.bop.rhs, keep_left);
    case EX_SUB:
      return sub_expr(NULL, expr->u.bop.lhs, expr->u.bop.rhs, keep_left);
    case EX_MUL:
    case EX_DIV:
    case EX_MOD:
    case EX_BITAND:
    case EX_BITOR:
    case EX_BITXOR:
      if (!cast_numbers(&expr->u.bop.lhs, &expr->u.bop.rhs, keep_left))
        parse_error(/*tok*/NULL, "Cannot use `%d' except numbers.", expr->type);

      expr->valType = expr->u.bop.lhs->valType;
      break;

    case EX_LSHIFT:
    case EX_RSHIFT:
      {
        enum eType t;
        if (!is_number(t = expr->u.bop.lhs->valType->type) ||
            !is_number(t = expr->u.bop.rhs->valType->type))
          parse_error(/*tok*/NULL, "Cannot use `%d' except numbers.", t);
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
      expr->u.bop.rhs = new_expr_cast(expr->valType, expr->u.bop.rhs, false);
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
        parse_error(/*tok*/NULL, "Cannot apply `+' except number types");
      return expr->u.unary.sub;

    case EX_NEG:
      if (!is_number(expr->u.unary.sub->valType->type))
        parse_error(/*tok*/NULL, "Cannot apply `-' except number types");
      expr->valType = expr->u.unary.sub->valType;
      break;

    case EX_NOT:
      switch (expr->u.unary.sub->valType->type) {
      case TY_CHAR:
      case TY_SHORT:
      case TY_INT:
      case TY_LONG:
      case TY_ENUM:
      case TY_PTR:
        break;
      default:
        parse_error(/*tok*/NULL, "Cannot apply `!' except number or pointer types");
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
          parse_error(NULL, "Cannot dereference raw type");
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
        if (!can_cast(expr->valType, sub->valType, sub, true))
          parse_error(NULL, "Cannot convert value from type %d to %d", sub->valType->type, expr->valType->type);
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
    if (!same_type(expr->u.ternary.tval->valType, expr->u.ternary.fval->valType))
      parse_error(NULL, "lhs and rhs must be same type");
    expr->valType = expr->u.ternary.tval->valType;
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
        if (!is_struct_or_union(targetType->type))
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
          p = new_expr_member(type, p, NULL, NULL, index);
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

      Vector *params = functype->u.func.params;  // <VarInfo*>
      if (params != NULL) {
        int argc = args != NULL ? args->len : 0;
        int paramc = params->len;
        if (!(argc == paramc ||
              (functype->u.func.vaargs && argc >= paramc)))
          parse_error(/*tok*/ NULL, "function `%s' expect %d arguments, but %d", func->u.varref.ident, paramc, argc);
      }

      if (args != NULL) {
        for (int i = 0, len = args->len; i < len; ++i) {
          if (params != NULL && i < params->len) {
            const Type *type = ((VarInfo*)params->data[i])->type;
            args->data[i] = new_expr_cast(type, args->data[i], false);
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
