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
static Expr *prim(void);

//

int var_find(Vector *lvars, const char *name) {
  for (int i = 0, len = lvars->len; i < len; ++i) {
    VarInfo *info = (VarInfo*)lvars->data[i];
    if (info->name != NULL && strcmp(info->name, name) == 0)
      return i;
  }
  return -1;
}

void var_add(Vector *lvars, const Token *ident, const Type *type, int flag) {
  const char *name = NULL;
  if (ident != NULL) {
    name = ident->u.ident;
    int idx = var_find(lvars, name);
    if (idx >= 0)
      parse_error(ident, "`%s' already defined", name);
    if (flag & VF_STATIC)  // TODO: Handle static specifier in local definition.
      parse_error(ident, "Cannot specify `static' (yet)");
  }

  VarInfo *info = malloc(sizeof(*info));
  info->name = name;
  info->type = type;
  info->flag = flag;
  info->offset = -1;
  vec_push(lvars, info);
}

// Struct

Map *struct_map;

// Typedef

Map *typedef_map;

// Global

Map *gvar_map;

GlobalVarInfo *find_global(const char *name) {
  return (GlobalVarInfo*)map_get(gvar_map, name);
}

void define_global(const Type *type, int flag, const Token *ident, Initializer *init) {
  const char *name = ident->u.ident;
  GlobalVarInfo *varinfo = find_global(name);
  if (varinfo != NULL && !(varinfo->flag & VF_EXTERN))
    parse_error(ident, "`%s' already defined", name);
  varinfo = malloc(sizeof(*varinfo));
  varinfo->name = name;
  varinfo->type = type;
  varinfo->flag = flag;
  varinfo->init = init;
  varinfo->offset = 0;
  map_put(gvar_map, name, varinfo);
}

// Type

// Call before accessing struct member to ensure that struct is declared.
void ensure_struct(Type *type, Token *token) {
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

static bool same_type(const Type *type1, const Type *type2) {
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

static Type* arrayof(const Type *type, size_t length) {
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

// Defun

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

void add_cur_scope(const Token *ident, const Type *type, int flag) {
  if (curscope->vars == NULL)
    curscope->vars = new_vector();
  var_add(curscope->vars, ident, type, flag);
}

//

Defun *curfunc;

Expr *new_expr(enum ExprType type , const Type *expType) {
  Expr *expr = malloc(sizeof(*expr));
  expr->type = type;
  expr->expType = expType;
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
      if (src_expr->type == ND_INT && src_expr->u.value == 0)  // Special handling for 0 to pointer.
        return true;
      break;
    case TY_LONG:
      if (src_expr->type == ND_LONG && src_expr->u.value == 0)  // Special handling for 0 to pointer.
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

Expr *new_node_cast(const Type *type, Expr *sub, bool is_explicit) {
  if (type->type == TY_VOID || sub->expType->type == TY_VOID)
    parse_error(NULL, "cannot use `void' as a value");

  if (!can_cast(type, sub->expType, sub, is_explicit))
    parse_error(NULL, "Cannot convert value from type %d to %d", sub->expType->type, type->type);

  if (same_type(type, sub->expType))
    return sub;

  Expr *node = new_expr(ND_CAST, type);
  node->u.cast.sub = sub;
  return node;
}

Expr *new_node_bop(enum NodeType type, const Type *expType, Expr *lhs, Expr *rhs) {
  Expr *node = new_expr(type, expType);
  node->u.bop.lhs = lhs;
  node->u.bop.rhs = rhs;
  return node;
}

static Expr *new_node_unary(enum NodeType type, const Type *expType, Expr *sub) {
  Expr *node = new_expr(type, expType);
  node->u.unary.sub = sub;
  return node;
}

Expr *new_node_deref(Expr *sub) {
  if (sub->expType->type != TY_PTR && sub->expType->type != TY_ARRAY)
    parse_error(NULL, "Cannot dereference raw type");
  return new_node_unary(ND_DEREF, sub->expType->u.pa.ptrof, sub);
}

Expr *new_node_numlit(enum ExprType nodetype, intptr_t val) {
  const Type *type = NULL;
  switch (nodetype) {
  case ND_CHAR:  type = &tyChar; break;
  case ND_SHORT: type = &tyShort; break;
  case ND_INT:   type = &tyInt; break;
  case ND_LONG:  type = &tyLong; break;
  default: assert(false); break;
  }
  Expr *node = new_expr(nodetype, type);
  node->u.value = val;
  return node;
}

static Expr *new_node_str(const char *str, size_t len) {
  Type *type = malloc(sizeof(*type));
  type->type = TY_ARRAY;
  type->u.pa.ptrof = &tyChar;
  type->u.pa.length = len;

  Expr *node = new_expr(ND_STR, type);
  node->u.str.buf = str;
  node->u.str.len = len;
  return node;
}

Expr *new_node_varref(const char *name, const Type *type, bool global) {
  Expr *node = new_expr(ND_VARREF, type);
  node->u.varref.ident = name;
  node->u.varref.global = global;
  return node;
}

Expr *new_node_member(Expr *target, int index, const Type *expType) {
  Expr *node = new_expr(ND_MEMBER, expType);
  node->u.member.target = target;
  node->u.member.index = index;
  return node;
}

static Expr *new_node_funcall(Expr *func, Vector *args) {
  const Type *rettype = func->expType->type == TY_FUNC ? func->expType->u.func.ret : &tyInt;  // TODO: Fix.
  Expr *node = new_expr(ND_FUNCALL, rettype);
  node->u.funcall.func = func;
  node->u.funcall.args = args;
  return node;
}

static Expr *new_node_ternary(Expr *cond, Expr *tval, Expr *fval, const Type *type) {
  Expr *node = new_expr(ND_TERNARY, type);
  node->u.ternary.cond = cond;
  node->u.ternary.tval = tval;
  node->u.ternary.fval = fval;
  return node;
}

static Expr *new_node_sizeof(const Type *type) {
  Expr *node = new_expr(ND_SIZEOF, &tySize);
  node->u.sizeof_.type = type;
  return node;
}

static Expr *funcall(Expr *func) {
  const Type *functype;

  if (!((functype = func->expType)->type == TY_FUNC ||
        (func->expType->type == TY_PTR && (functype = func->expType->u.pa.ptrof)->type == TY_FUNC)))
    parse_error(NULL, "Cannot call except funtion");

  Vector *args = NULL;
  Token *tok;
  if ((tok = consume(TK_RPAR)) == NULL) {
    args = new_vector();
    for (;;) {
      Expr *arg = expr();
      if (functype->u.func.params != NULL && args->len < functype->u.func.params->len) {
        const Type * type = ((VarInfo*)functype->u.func.params->data[args->len])->type;
        arg = new_node_cast(type, arg, false);
      }
      vec_push(args, arg);
      if ((tok = consume(TK_RPAR)) != NULL)
        break;
      if (consume(TK_COMMA))
        continue;
      parse_error(NULL, "Comma or `)` expected");
    }
  }

  if (functype->u.func.params != NULL) {
    int argc = args != NULL ? args->len : 0;
    int paramc = functype->u.func.params->len;
    if (!(argc == paramc ||
          (functype->u.func.vaargs && argc >= paramc)))
     parse_error(tok, "function `%s' expect %d arguments, but %d", func->u.varref.ident, paramc, argc);
  }

  return new_node_funcall(func, args);
}

Expr *add_node(Token *tok, Expr *lhs, Expr *rhs) {
  Expr *l = lhs, *r = rhs;

  if (lhs->expType->type > rhs->expType->type) {
    Expr *tmp = l;
    l = r;
    r = tmp;
  }

  switch (l->expType->type) {
  case TY_CHAR:
    switch (r->expType->type) {
    case TY_CHAR:
      return new_node_bop(ND_ADD, l->expType, l, r);
    case TY_INT:
      return new_node_bop(ND_ADD, r->expType, new_node_cast(r->expType, l, false), r);
    case TY_LONG:
      return new_node_bop(ND_ADD, r->expType, new_node_cast(r->expType, l, false), r);
    default:
      break;
    }
    break;

  case TY_SHORT:
    switch (r->expType->type) {
    case TY_SHORT:
      return new_node_bop(ND_ADD, l->expType, l, r);
    case TY_INT:
    case TY_LONG:
      return new_node_bop(ND_ADD, l->expType, new_node_cast(r->expType, l, false), r);
    case TY_PTR:
      return new_node_bop(ND_PTRADD, r->expType, r, new_node_cast(&tySize, l, false));
    case TY_ARRAY:
      return new_node_bop(ND_PTRADD, array_to_ptr(r->expType), r,  new_node_cast(&tySize, l, false));
    default:
      break;
    }
    break;

  case TY_INT:
    switch (r->expType->type) {
    case TY_INT:
      return new_node_bop(ND_ADD, l->expType, l, r);
    case TY_LONG:
      return new_node_bop(ND_ADD, r->expType, new_node_cast(r->expType, l, false), r);
    case TY_PTR:
      return new_node_bop(ND_PTRADD, r->expType, r, new_node_cast(&tySize, l, false));
    case TY_ARRAY:
      return new_node_bop(ND_PTRADD, array_to_ptr(r->expType), r,  new_node_cast(&tySize, l, false));
    default:
      break;
    }
    break;

  case TY_LONG:
    switch (r->expType->type) {
    case TY_LONG:
      return new_node_bop(ND_ADD, l->expType, l, r);
    case TY_PTR:
      return new_node_bop(ND_PTRADD, r->expType, r, l);
    case TY_ARRAY:
      return new_node_bop(ND_PTRADD, array_to_ptr(r->expType), r, l);
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

static Expr *sub_node(Token *tok, Expr *lhs, Expr *rhs) {
  switch (lhs->expType->type) {
  case TY_CHAR:
    switch (rhs->expType->type) {
    case TY_CHAR:
    case TY_INT:
    case TY_LONG:
      return new_node_bop(ND_SUB, rhs->expType, new_node_cast(rhs->expType, lhs, false), rhs);
    default:
      break;
    }
    break;

  case TY_SHORT:
    switch (rhs->expType->type) {
    case TY_CHAR:
    case TY_SHORT:
      return new_node_bop(ND_SUB, lhs->expType, lhs, new_node_cast(lhs->expType, rhs, false));
    case TY_INT:
    case TY_LONG:
      return new_node_bop(ND_SUB, rhs->expType, new_node_cast(rhs->expType, lhs, false), rhs);
    default:
      break;
    }
    break;

  case TY_INT:
    switch (rhs->expType->type) {
    case TY_CHAR:
    case TY_SHORT:
    case TY_INT:
      return new_node_bop(ND_SUB, lhs->expType, lhs, new_node_cast(lhs->expType, rhs, false));
    case TY_LONG:
      return new_node_bop(ND_SUB, rhs->expType, new_node_cast(rhs->expType, lhs, false), rhs);
    default:
      break;
    }
    break;

  case TY_LONG:
    switch (rhs->expType->type) {
    case TY_CHAR:
    case TY_INT:
    case TY_LONG:
      return new_node_bop(ND_SUB, lhs->expType, lhs, new_node_cast(lhs->expType, rhs, false));
    default:
      break;
    }
    break;

  case TY_PTR:
    switch (rhs->expType->type) {
    case TY_INT:
    case TY_LONG:
      return new_node_bop(ND_PTRSUB, lhs->expType, lhs, rhs);
    case TY_PTR:
      if (!same_type(lhs->expType, rhs->expType))
        parse_error(tok, "Different pointer sub");
      return new_node_bop(ND_PTRDIFF, &tyInt, lhs, rhs);  // TODO: size_t
    case TY_ARRAY:
      if (!same_type(lhs->expType->u.pa.ptrof, rhs->expType->u.pa.ptrof))
        parse_error(tok, "Different pointer sub");
      return new_node_bop(ND_PTRDIFF, &tyInt, lhs, rhs);  // TODO: size_t
    default:
      break;
    }
    break;

  case TY_ARRAY:
    switch (rhs->expType->type) {
    case TY_PTR:
      if (!same_type(lhs->expType->u.pa.ptrof, rhs->expType->u.pa.ptrof))
        parse_error(tok, "Different pointer sub");
      return new_node_bop(ND_PTRDIFF, &tyInt, lhs, rhs);  // TODO: size_t
    case TY_ARRAY:
      if (!same_type(lhs->expType, rhs->expType))
        parse_error(tok, "Different pointer sub");
      return new_node_bop(ND_PTRDIFF, &tyInt, lhs, rhs);  // TODO: size_t
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

static Expr *arith_node(Token *tok, enum ExprType nodeType, Expr *lhs, Expr *rhs, bool cast) {
  if (!is_number(lhs->expType->type) || !is_number(rhs->expType->type))
    parse_error(tok, "Cannot use `%d' except numbers.", nodeType);

  const Type *expType = lhs->expType;
  if (cast && rhs->expType->type > expType->type) {
    expType = rhs->expType;
    lhs = new_node_cast(expType, lhs, false);
  }

  return new_node_bop(nodeType, expType, lhs, rhs);
}

Expr *array_index(Expr *array) {
  Expr *index = expr();
  Token *tok;
  if ((tok = consume(TK_RBRACKET)) == NULL)
    parse_error(NULL, "`]' expected");
  return new_node_deref(add_node(tok, array, index));
}

bool member_access_recur(const Type *type, Token *ident, Vector *stack) {
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
  // Find member's type from struct info.
  const Type *type = target->expType;
  if (acctok->type == TK_DOT) {
    if (!is_struct_or_union(type->type))
      parse_error(acctok, "`.' for non struct value");
  } else {  // TK_ARROW
    if (type->type == TY_PTR)
      type = target->expType->u.pa.ptrof;
    else if (type->type == TY_ARRAY)
      type = target->expType->u.pa.ptrof;
    else
      parse_error(acctok, "`->' for non pointer value");
    if (type->type != TY_STRUCT)
      parse_error(acctok, "`->' for non struct value");
  }

  Token *ident;
  if (!(ident = consume(TK_IDENT)))
    parse_error(NULL, "`ident' expected");
  const char *name = ident->u.ident;

  ensure_struct((Type*)type, ident);
  int index = var_find(type->u.struct_.info->members, name);
  if (index >= 0) {
    VarInfo *varinfo = (VarInfo*)type->u.struct_.info->members->data[index];
    return new_node_member(target, index, varinfo->type);
  }

  Vector *stack = new_vector();
  bool res = member_access_recur(type, ident, stack);
  if (!res)
    parse_error(ident, "`%s' doesn't exist in the struct", name);
  Expr *node = target;
  for (int i = 0; i < stack->len; ++i) {
    int index = (int)(long)stack->data[i];
    VarInfo *varinfo = type->u.struct_.info->members->data[index];
    node = new_node_member(node, index, varinfo->type);
    type = varinfo->type;
  }
  return node;
}

static const Type *parse_enum(void) {
  Token *typeident = consume(TK_IDENT);

  if (consume(TK_LBRACE)) {
    if (!consume(TK_RBRACE)) {
      int value = 0;
      for (;;) {
        Token *ident = consume(TK_IDENT);
        if (ident == NULL)
          parse_error(NULL, "ident expected");
        if (consume(TK_ASSIGN)) {
          Token *tok = fetch_token();
          Expr *node = expr();
          if (node->type != ND_INT)  // TODO: Accept constexpr.
            parse_error(tok, "const expected for enum");
          value = node->u.value;
        }
        // Define
        (void)typeident;  // TODO: Define enum type with name.
        Initializer *init = malloc(sizeof(*init));
        init->type = vSingle;
        init->u.single = new_node_numlit(ND_INT, value);
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
  Token *tok;
  size_t length = -1;
  if (consume(TK_RBRACKET)) {
    // Arbitrary size.
  } else if ((tok = consume(TK_INTLIT))) {  // TODO: Constant expression.
    if (tok->u.value <= 0)
      parse_error(tok, "Array size must be greater than 0, but %d", (int)tok->u.value);
    length = tok->u.value;
    if (!consume(TK_RBRACKET))
      parse_error(NULL, "`]' expected");
  } else {
    parse_error(NULL, "syntax error");
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

    const Type *type;
    int flag;
    Token *ident;
    if (!parse_var_def(NULL, &type, &flag, &ident))
      parse_error(NULL, "type expected");
    not_void(type);

    if (!consume(TK_SEMICOL))
      parse_error(NULL, "`;' expected");
    var_add(members, ident, type, flag);
  }

  StructInfo *sinfo = malloc(sizeof(*sinfo));
  sinfo->members = members;
  sinfo->is_union = is_union;
  sinfo->size = -1;
  sinfo->align = 0;
  return sinfo;
}

static Expr *parse_sizeof(void) {
  const Type *type;
  if (consume(TK_LPAR)) {
    type = parse_full_type(NULL, NULL);
    if (type == NULL) {
      Expr *node = expr();
      type = node->expType;
    }
    if (!consume(TK_RPAR))
      parse_error(NULL, "`)' expected");
  } else {
    Expr *node = prim();
    type = node->expType;
  }
  return new_node_sizeof(type);
}

static Expr *prim(void) {
  if (consume(TK_LPAR)) {
    Expr *node = expr();
    if (!consume(TK_RPAR))
      parse_error(NULL, "No close paren");
    return node;
  }

  Token *tok;
  {
    enum ExprType nt;
    if (((tok = consume(TK_CHARLIT)) != NULL && (nt = ND_CHAR, true)) ||
        ((tok = consume(TK_INTLIT)) != NULL && (nt = ND_INT, true)) ||
        ((tok = consume(TK_LONGLIT)) != NULL && (nt = ND_LONG, true)))
      return new_node_numlit(nt, tok->u.value);
  }
  if ((tok = consume(TK_STR)))
    return new_node_str(tok->u.str.buf, tok->u.str.len);

  Token *ident;
  if ((ident = consume(TK_IDENT)) != NULL) {
    const char *name = ident->u.ident;
    const Type *type = NULL;
    bool global = false;
    if (curfunc != NULL) {
      VarInfo *varinfo = scope_find(curscope, name);
      if (varinfo != NULL)
        type = varinfo->type;
    }
    if (type == NULL) {
      GlobalVarInfo *varinfo = find_global(name);
      if (varinfo != NULL) {
        global = true;
        type = varinfo->type;
        if (type->type == TY_ENUM) {
          // Enum value is embeded directly.
          assert(varinfo->init->type == vSingle);
          return varinfo->init->u.single;
        }
      }
    }
    if (type == NULL)
      parse_error(ident, "Undefined `%s'", name);

    return new_node_varref(name, type, global);
  }
  parse_error(NULL, "Number or Ident or open paren expected");
  return NULL;
}

static Expr *postfix(void) {
  Expr *node = prim();

  for (;;) {
    Token *tok;
    if (consume(TK_LPAR))
      node = funcall(node);
    else if (consume(TK_LBRACKET))
      node = array_index(node);
    else if ((tok = consume(TK_DOT)) != NULL)
      node = member_access(node, tok);
    else if ((tok = consume(TK_ARROW)) != NULL)
      node = member_access(node, tok);
    else if (consume(TK_INC))
      node = new_node_unary(ND_POSTINC, node->expType, node);
    else if (consume(TK_DEC))
      node = new_node_unary(ND_POSTDEC, node->expType, node);
    else
      return node;
  }
}

static Expr *unary(void) {
  Token *tok;
  if ((tok = consume(TK_ADD)) != NULL) {
    Expr *node = cast_expr();
    if (!is_number(node->expType->type))
      parse_error(tok, "Cannot apply `+' except number types");
    return node;
  }

  if ((tok = consume(TK_SUB)) != NULL) {
    Expr *node = cast_expr();
    if (!is_number(node->expType->type))
      parse_error(tok, "Cannot apply `-' except number types");
    switch (node->type) {
    case ND_CHAR:
    case ND_INT:
    case ND_LONG:
      node->u.value = -node->u.value;
      return node;
    default:
      return new_node_unary(ND_NEG, node->expType, node);
    }
  }

  if ((tok = consume(TK_NOT)) != NULL) {
    Expr *node = cast_expr();
    switch (node->expType->type) {
    case TY_INT:
    case TY_CHAR:
    case TY_LONG:
    case TY_PTR:
      node = new_node_unary(ND_NOT, &tyBool, node);
      break;
    default:
      parse_error(tok, "Cannot apply `!' except number or pointer types");
      break;
    }
    return node;
  }

  if (consume(TK_AND)) {
    Expr *node = cast_expr();
    return new_node_unary(ND_REF, ptrof(node->expType), node);
  }

  if (consume(TK_MUL)) {
    Expr *node = cast_expr();
    return new_node_deref(node);
  }

  if (consume(TK_INC)) {
    Expr *node = unary();
    return new_node_unary(ND_PREINC, node->expType, node);
  }

  if (consume(TK_DEC)) {
    Expr *node = unary();
    return new_node_unary(ND_PREDEC, node->expType, node);
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
      Expr *node = cast_expr();
      return new_node_cast(type, node, true);
    }
    unget_token(lpar);
  }
  return unary();
}

static Expr *mul(void) {
  Expr *node = cast_expr();

  for (;;) {
    enum ExprType t;
    Token *tok;
    if ((tok = consume(TK_MUL)) != NULL)
      t = ND_MUL;
    else if ((tok = consume(TK_DIV)) != NULL)
      t = ND_DIV;
    else if ((tok = consume(TK_MOD)) != NULL)
      t = ND_MOD;
    else
      return node;

    node = arith_node(tok, t, node, cast_expr(), true);
  }
}

static Expr *add(void) {
  Expr *node = mul();

  for (;;) {
    Token *tok;
    if ((tok = consume(TK_ADD)) != NULL)
      node = add_node(tok, node, mul());
    else if ((tok = consume(TK_SUB)) != NULL)
      node = sub_node(tok, node, mul());
    else
      return node;
  }
}

static Expr *shift(void) {
  Expr *node = add();

  for (;;) {
    enum ExprType t;
    Token *tok;
    if ((tok = consume(TK_LSHIFT)) != NULL)
      t = ND_LSHIFT;
    else if ((tok = consume(TK_RSHIFT)) != NULL)
      t = ND_RSHIFT;
    else
      return node;

    Expr *lhs = node, *rhs = add();
    node = arith_node(tok, t, lhs, rhs, false);
  }
}

static bool cast_numbers(Expr **pLhs, Expr **pRhs) {
  enum eType ltype = (*pLhs)->expType->type, rtype = (*pRhs)->expType->type;
  if (!is_number(ltype) || !is_number(rtype))
    return false;

  if (ltype < rtype)
    *pLhs = new_node_cast((*pRhs)->expType, *pLhs, false);
  else if (ltype > rtype)
    *pRhs = new_node_cast((*pLhs)->expType, *pRhs, false);
  return true;
}

static Expr *cmp(void) {
  Expr *node = shift();

  for (;;) {
    enum ExprType t;
    Token *tok;
    if ((tok = consume(TK_LT)) != NULL)
      t = ND_LT;
    else if ((tok = consume(TK_GT)) != NULL)
      t = ND_GT;
    else if ((tok = consume(TK_LE)) != NULL)
      t = ND_LE;
    else if ((tok = consume(TK_GE)) != NULL)
      t = ND_GE;
    else
      return node;

    Expr *lhs = node, *rhs= shift();
    if (lhs->expType->type == TY_PTR || rhs->expType->type == TY_PTR) {
      const Type *lt = lhs->expType, *rt = rhs->expType;
      if (lt->type != TY_PTR) {
        const Type *tmp = lt;
        lt = rt;
        rt = tmp;
      }
      if (!can_cast(lt, rt, rhs, false))
        parse_error(tok, "Cannot compare pointer to other types");
      if (rt->type != TY_PTR) {
        if (lt == lhs->expType)
          rhs = new_node_cast(lhs->expType, rhs, false);
        else
          lhs = new_node_cast(rhs->expType, lhs, false);
      }
    } else {
      if (!cast_numbers(&lhs, &rhs))
        parse_error(tok, "Cannot compare except numbers");
    }
    node = new_node_bop(t, &tyBool, lhs, rhs);
  }
}

static Expr *eq(void) {
  Expr *node = cmp();

  for (;;) {
    enum ExprType t;
    Token *tok;
    if ((tok = consume(TK_EQ)) != NULL)
      t = ND_EQ;
    else if ((tok = consume(TK_NE)) != NULL)
      t = ND_NE;
    else
      return node;

    Expr *lhs = node, *rhs= cmp();
    if (lhs->expType->type == TY_PTR || rhs->expType->type == TY_PTR) {
      const Type *lt = lhs->expType, *rt = rhs->expType;
      if (lt->type != TY_PTR) {
        const Type *tmp = lt;
        lt = rt;
        rt = tmp;
      }
      if (!can_cast(lt, rt, rhs, false))
        parse_error(tok, "Cannot compare pointer to other types");
      if (rt->type != TY_PTR) {
        if (lt == lhs->expType)
          rhs = new_node_cast(lhs->expType, rhs, false);
        else
          lhs = new_node_cast(rhs->expType, lhs, false);
      }
    } else {
      if (!cast_numbers(&lhs, &rhs))
        parse_error(tok, "Cannot compare except numbers");
    }
    node = new_node_bop(t, &tyBool, lhs, rhs);
  }
}

static Expr *and(void) {
  Expr *node = eq();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_AND)) != NULL) {
      Expr *lhs = node, *rhs= eq();
      if (!cast_numbers(&lhs, &rhs))
        parse_error(tok, "Cannot compare except numbers");
      node = new_node_bop(ND_BITAND, lhs->expType, lhs, rhs);
    } else
      return node;
  }
}

static Expr *xor(void) {
  Expr *node = and();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_HAT)) != NULL) {
      Expr *lhs = node, *rhs= and();
      if (!cast_numbers(&lhs, &rhs))
        parse_error(tok, "Cannot compare except numbers");
      node = new_node_bop(ND_BITXOR, lhs->expType, lhs, rhs);
    } else
      return node;
  }
}

static Expr *or(void) {
  Expr *node = xor();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_OR)) != NULL) {
      Expr *lhs = node, *rhs= xor();
      if (!cast_numbers(&lhs, &rhs))
        parse_error(tok, "Cannot compare except numbers");
      node = new_node_bop(ND_BITOR, lhs->expType, lhs, rhs);
    } else
      return node;
  }
}

static Expr *logand(void) {
  Expr *node = or();
  for (;;) {
    if (consume(TK_LOGAND))
      node = new_node_bop(ND_LOGAND, &tyBool, node, or());
    else
      return node;
  }
}

static Expr *logior(void) {
  Expr *node = logand();
  for (;;) {
    if (consume(TK_LOGIOR))
      node = new_node_bop(ND_LOGIOR, &tyBool, node, logand());
    else
      return node;
  }
}

static Expr *conditional(void) {
  Expr *node = logior();
  for (;;) {
    if (!consume(TK_QUESTION))
      return node;
    Expr *t = expr();
    if (!consume(TK_COLON))
      parse_error(NULL, "`:' expected");
    Expr *f = conditional();
    if (!same_type(t->expType, f->expType))
      parse_error(NULL, "lhs and rhs must be same type");
    node = new_node_ternary(node, t, f, t->expType);
  }
}

static Expr *assign(void) {
  Expr *node = conditional();

  if (consume(TK_ASSIGN))
    return new_node_bop(ND_ASSIGN, node->expType, node, new_node_cast(node->expType, assign(), false));
  Token *tok;
  if ((tok = consume(TK_ADD_ASSIGN)) != NULL)
    return new_node_unary(ND_ASSIGN_WITH, node->expType,
                          add_node(tok, node, assign()));
  if ((tok = consume(TK_SUB_ASSIGN)) != NULL)
    return new_node_unary(ND_ASSIGN_WITH, node->expType,
                          sub_node(tok, node, assign()));
  if ((tok = consume(TK_MUL_ASSIGN)) != NULL)
    return new_node_unary(ND_ASSIGN_WITH, node->expType,
                          arith_node(tok, ND_MUL, node, assign(), true));
  if ((tok = consume(TK_DIV_ASSIGN)) != NULL)
    return new_node_unary(ND_ASSIGN_WITH, node->expType,
                          arith_node(tok, ND_DIV, node, assign(), true));
  if ((tok = consume(TK_MOD_ASSIGN)) != NULL)
    return new_node_unary(ND_ASSIGN_WITH, node->expType,
                          arith_node(tok, ND_MOD, node, assign(), true));

  return node;
}

Expr *expr(void) {
  return assign();
}
