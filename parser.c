#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "xcc.h"
#include "util.h"

static const Type tyVoid = {.type=TY_VOID};
static const Type tyChar = {.type=TY_CHAR};
static const Type tyShort = {.type=TY_SHORT};
static const Type tyInt = {.type=TY_INT};
static const Type tyLong = {.type=TY_LONG};
static const Type tyEnum = {.type=TY_ENUM};
#define tyBool  tyInt
#define tySize  tyLong

static StructInfo *parse_struct(bool is_union);
static Node *stmt(void);
static Node *expr(void);
static Node *cast_expr(void);
static Node *prim(void);
static Vector *funparams(void);

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

Map *global;

GlobalVarInfo *find_global(const char *name) {
  return (GlobalVarInfo*)map_get(global, name);
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
  map_put(global, name, varinfo);
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

static bool is_struct_or_union(enum eType type) {
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

static Type* new_func_type(const Type *ret, const Vector *params) {
  Type *f = malloc(sizeof(*f));
  f->type = TY_FUNC;
  f->u.func.ret = ret;

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

static void scope_add(Scope *scope, const Token *ident, const Type *type, int flag) {
  if (scope->vars == NULL)
    scope->vars = new_vector();
  var_add(scope->vars, ident, type, flag);
}

// Defun

static Scope *curscope;

static Scope *enter_scope(Defun *defun, Vector *vars) {
  Scope *scope = new_scope(curscope, vars);
  curscope = scope;
  vec_push(defun->all_scopes, scope);
  return scope;
}

static void exit_scope(void) {
  assert(curscope != NULL);
  curscope = curscope->parent;
}

static Defun *new_defun(const Type *type, const char *name) {
  Defun *defun = malloc(sizeof(*defun));
  defun->type = type;
  defun->name = name;
  defun->top_scope = NULL;
  defun->stmts = NULL;
  defun->all_scopes = new_vector();
  defun->ret_label = NULL;
  return defun;
}

//

const int LF_BREAK = 1 << 0;
const int LF_CONTINUE = 1 << 0;

static Defun *curfunc;
static int curloopflag;
static Node *curswitch;

static Node *new_node(enum NodeType type, const Type *expType) {
  Node *node = malloc(sizeof(Node));
  node->type = type;
  node->expType = expType;
  return node;
}

static bool can_cast(const Type *dst, const Type *src, Node *src_node, bool is_explicit) {
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
      if (src_node->type == ND_INT && src_node->u.value == 0)  // Special handling for 0 to pointer.
        return true;
      break;
    case TY_LONG:
      if (src_node->type == ND_LONG && src_node->u.value == 0)  // Special handling for 0 to pointer.
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
          can_cast(dst, ptrof(src->u.pa.ptrof), src_node, is_explicit))
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

static Node *new_node_cast(const Type *type, Node *sub, bool is_explicit) {
  if (type->type == TY_VOID || sub->expType->type == TY_VOID)
    parse_error(NULL, "cannot use `void' as a value");

  if (!can_cast(type, sub->expType, sub, is_explicit))
    parse_error(NULL, "Cannot convert value from type %d to %d", sub->expType->type, type->type);

  if (same_type(type, sub->expType))
    return sub;

  Node *node = new_node(ND_CAST, type);
  node->u.cast.sub = sub;
  return node;
}

static Node *new_node_bop(enum NodeType type, const Type *expType, Node *lhs, Node *rhs) {
  Node *node = new_node(type, expType);
  node->u.bop.lhs = lhs;
  node->u.bop.rhs = rhs;
  return node;
}

static Node *new_node_unary(enum NodeType type, const Type *expType, Node *sub) {
  Node *node = new_node(type, expType);
  node->u.unary.sub = sub;
  return node;
}

static Node *new_node_deref(Node *sub) {
  if (sub->expType->type != TY_PTR && sub->expType->type != TY_ARRAY)
    parse_error(NULL, "Cannot dereference raw type");
  return new_node_unary(ND_DEREF, sub->expType->u.pa.ptrof, sub);
}

static Node *new_node_numlit(enum NodeType nodetype, intptr_t val) {
  const Type *type = NULL;
  switch (nodetype) {
  case ND_CHAR:  type = &tyChar; break;
  case ND_SHORT: type = &tyShort; break;
  case ND_INT:   type = &tyInt; break;
  case ND_LONG:  type = &tyLong; break;
  default: assert(false); break;
  }
  Node *node = new_node(nodetype, type);
  node->u.value = val;
  return node;
}

static Node *new_node_str(const char *str, size_t len) {
  Type *type = malloc(sizeof(*type));
  type->type = TY_ARRAY;
  type->u.pa.ptrof = &tyChar;
  type->u.pa.length = len;

  Node *node = new_node(ND_STR, type);
  node->u.str.buf = str;
  node->u.str.len = len;
  return node;
}

static Node *new_node_varref(const char *name, const Type *type, bool global) {
  Node *node = new_node(ND_VARREF, type);
  node->u.varref.ident = name;
  node->u.varref.global = global;
  return node;
}

static Node *new_node_member(Node *target, const char *name, const Type *expType) {
  Node *node = new_node(ND_MEMBER, expType);
  node->u.member.target = target;
  node->u.member.name = name;
  return node;
}

static Node *new_node_defun(Defun *defun) {
  Node *node = new_node(ND_DEFUN, &tyVoid);
  node->u.defun = defun;
  return node;
}

static Node *new_node_funcall(Node *func, Vector *args) {
  const Type *rettype = func->expType->type == TY_FUNC ? func->expType->u.func.ret : &tyInt;  // TODO: Fix.
  Node *node = new_node(ND_FUNCALL, rettype);
  node->u.funcall.func = func;
  node->u.funcall.args = args;
  return node;
}

static Node *new_node_block(Scope *scope, Vector *nodes) {
  Node *node = new_node(ND_BLOCK, &tyVoid);
  node->u.block.scope = scope;
  node->u.block.nodes = nodes;
  return node;
}

static Node *new_node_if(Node *cond, Node *tblock, Node *fblock, const Type *type) {
  Node *node = new_node(ND_IF, type);
  node->u.if_.cond = cond;
  node->u.if_.tblock = tblock;
  node->u.if_.fblock = fblock;
  return node;
}

static Node *new_node_switch(Node *value) {
  Node *node = new_node(ND_SWITCH, &tyVoid);
  node->u.switch_.value = value;
  node->u.switch_.body = NULL;
  node->u.switch_.case_values = NULL;
  node->u.switch_.has_default = false;
  return node;
}

static Node *new_node_case(int value) {
  Node *node = new_node(ND_LABEL, &tyVoid);
  node->u.label.type = lCASE;
  node->u.label.u.case_value = value;
  return node;
}

static Node *new_node_default(void) {
  Node *node = new_node(ND_LABEL, &tyVoid);
  node->u.label.type = lDEFAULT;
  return node;
}

static Node *new_node_while(Node *cond, Node *body) {
  Node *node = new_node(ND_WHILE, &tyVoid);
  node->u.while_.cond = cond;
  node->u.while_.body = body;
  return node;
}

static Node *new_node_do_while(Node *body, Node *cond) {
  Node *node = new_node(ND_DO_WHILE, &tyVoid);
  node->u.do_while.body = body;
  node->u.do_while.cond = cond;
  return node;
}

static Node *new_node_for(Node *pre, Node *cond, Node *post, Node *body) {
  Node *node = new_node(ND_FOR, &tyVoid);
  node->u.for_.pre = pre;
  node->u.for_.cond = cond;
  node->u.for_.post = post;
  node->u.for_.body = body;
  return node;
}

static Node *new_node_return(Node *val) {
  const Type *type = val != NULL ? val->expType : &tyVoid;
  Node *node = new_node(ND_RETURN, type);
  node->u.return_.val = val;
  return node;
}

static Node *new_node_sizeof(const Type *type) {
  Node *node = new_node(ND_SIZEOF, &tySize);
  node->u.sizeof_.type = type;
  return node;
}

static Node *funcall(Node *func) {
  const Type *functype;

  if (!((functype = func->expType)->type == TY_FUNC ||
        (func->expType->type == TY_PTR && (functype = func->expType->u.pa.ptrof)->type == TY_FUNC)))
    parse_error(NULL, "Cannot call except funtion");

  Vector *args = NULL;
  Token *tok;
  if ((tok = consume(TK_RPAR)) == NULL) {
    args = new_vector();
    for (;;) {
      Node *arg = expr();
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

  if (functype->u.func.params != NULL && (args != NULL ? args->len : 0) != functype->u.func.params->len)
    parse_error(tok, "function `%s' expect %d arguments, but %d", func->u.varref.ident, functype->u.func.params->len, (args != NULL ? args->len : 0));

  return new_node_funcall(func, args);
}

static Node *add_node(Token *tok, Node *lhs, Node *rhs) {
  Node *l = lhs, *r = rhs;

  if (lhs->expType->type > rhs->expType->type) {
    Node *tmp = l;
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

static Node *sub_node(Token *tok, Node *lhs, Node *rhs) {
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

static Node *arith_node(Token *tok, enum NodeType nodeType, Node *lhs, Node *rhs, bool cast) {
  if (!is_number(lhs->expType->type) || !is_number(rhs->expType->type))
    parse_error(tok, "Cannot use `%d' except numbers.", nodeType);

  const Type *expType = lhs->expType;
  if (cast && rhs->expType->type > expType->type) {
    expType = rhs->expType;
    lhs = new_node_cast(expType, lhs, false);
  }

  return new_node_bop(nodeType, expType, lhs, rhs);
}

Node *array_index(Node *array) {
  Node *index = expr();
  Token *tok;
  if ((tok = consume(TK_RBRACKET)) == NULL)
    parse_error(NULL, "`]' expected");
  return new_node_deref(add_node(tok, array, index));
}

Node *member_access(Node *target, Token *acctok) {
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
  if (index < 0)
    parse_error(ident, "`%s' doesn't exist in the struct", name);
  VarInfo *varinfo = (VarInfo*)type->u.struct_.info->members->data[index];

  return new_node_member(target, name, varinfo->type);
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
          Node *node = expr();
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

static const Type *parse_raw_type(int *pflag) {
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

static void not_void(const Type *type) {
  if (type->type == TY_VOID)
    parse_error(NULL, "`void' not allowed");
}

static const Type *parse_type_modifier(const Type* type) {
  if (type == NULL)
    return NULL;

  while (consume(TK_MUL))
    type = ptrof(type);

  return type;
}

static const Type *parse_type_suffix(const Type *type) {
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

static bool parse_var_def(const Type **prawType, const Type** ptype, int *pflag, Token **pident, bool allow_noname) {
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
    if (ident == NULL && !allow_noname)
      parse_error(NULL, "Ident expected");
    if (!consume(TK_RPAR))
      parse_error(NULL, "`)' expected");
    if (!consume(TK_LPAR))
      parse_error(NULL, "`(' expected");

    Vector *params = funparams();
    type = ptrof(new_func_type(type, params));
  } else {
    if (type->type != TY_VOID) {
      ident = consume(TK_IDENT);
      if (ident == NULL && !allow_noname)
        parse_error(NULL, "Ident expected");
    }
  }
  if (type->type != TY_VOID)
    type = parse_type_suffix(type);

  *ptype = type;
  if (pident != NULL)
    *pident = ident;

  return true;
}

static const Type *parse_full_type(int *pflag, Token **pident) {
  const Type *type;
  if (!parse_var_def(NULL, &type, pflag, pident, true))
    return NULL;
  return type;
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
    if (!parse_var_def(NULL, &type, &flag, &ident, false))
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

static Node *parse_sizeof(void) {
  const Type *type;
  if (consume(TK_LPAR)) {
    type = parse_full_type(NULL, NULL);
    if (type == NULL) {
      Node *node = expr();
      type = node->expType;
    }
    if (!consume(TK_RPAR))
      parse_error(NULL, "`)' expected");
  } else {
    Node *node = prim();
    type = node->expType;
  }
  return new_node_sizeof(type);
}

static Node *prim(void) {
  if (consume(TK_LPAR)) {
    Node *node = expr();
    if (!consume(TK_RPAR))
      parse_error(NULL, "No close paren");
    return node;
  }

  Token *tok;
  {
    enum NodeType nt;
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

static Node *postfix(void) {
  Node *node = prim();

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

static Node *unary(void) {
  Token *tok;
  if ((tok = consume(TK_ADD)) != NULL) {
    Node *node = cast_expr();
    if (!is_number(node->expType->type))
      parse_error(tok, "Cannot apply `+' except number types");
    return node;
  }

  if ((tok = consume(TK_SUB)) != NULL) {
    Node *node = cast_expr();
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
    Node *node = cast_expr();
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
    Node *node = cast_expr();
    return new_node_unary(ND_REF, ptrof(node->expType), node);
  }

  if (consume(TK_MUL)) {
    Node *node = cast_expr();
    return new_node_deref(node);
  }

  if (consume(TK_INC)) {
    Node *node = unary();
    return new_node_unary(ND_PREINC, node->expType, node);
  }

  if (consume(TK_DEC)) {
    Node *node = unary();
    return new_node_unary(ND_PREDEC, node->expType, node);
  }

  if (consume(TK_SIZEOF)) {
    return parse_sizeof();
  }

  return postfix();
}

static Node *cast_expr(void) {
  Token *lpar;
  if ((lpar = consume(TK_LPAR)) != NULL) {
    int flag;
    const Type *type = parse_full_type(&flag, NULL);
    if (type != NULL) {  // Cast
      if (!consume(TK_RPAR))
        parse_error(NULL, "`)' expected");
      Node *node = cast_expr();
      return new_node_cast(type, node, true);
    }
    unget_token(lpar);
  }
  return unary();
}

static Node *mul(void) {
  Node *node = cast_expr();

  for (;;) {
    enum NodeType t;
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

static Node *add(void) {
  Node *node = mul();

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

static Node *shift(void) {
  Node *node = add();

  for (;;) {
    enum NodeType t;
    Token *tok;
    if ((tok = consume(TK_LSHIFT)) != NULL)
      t = ND_LSHIFT;
    else if ((tok = consume(TK_RSHIFT)) != NULL)
      t = ND_RSHIFT;
    else
      return node;

    Node *lhs = node, *rhs = add();
    node = arith_node(tok, t, lhs, rhs, false);
  }
}

static bool cast_numbers(Node **pLhs, Node **pRhs) {
  enum eType ltype = (*pLhs)->expType->type, rtype = (*pRhs)->expType->type;
  if (!is_number(ltype) || !is_number(rtype))
    return false;

  if (ltype < rtype)
    *pLhs = new_node_cast((*pRhs)->expType, *pLhs, false);
  else if (ltype > rtype)
    *pRhs = new_node_cast((*pLhs)->expType, *pRhs, false);
  return true;
}

static Node *cmp(void) {
  Node *node = shift();

  for (;;) {
    enum NodeType t;
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

    Node *lhs = node, *rhs= shift();
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

static Node *eq(void) {
  Node *node = cmp();

  for (;;) {
    enum NodeType t;
    Token *tok;
    if ((tok = consume(TK_EQ)) != NULL)
      t = ND_EQ;
    else if ((tok = consume(TK_NE)) != NULL)
      t = ND_NE;
    else
      return node;

    Node *lhs = node, *rhs= cmp();
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

static Node *and(void) {
  Node *node = eq();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_AND)) != NULL) {
      Node *lhs = node, *rhs= eq();
      if (!cast_numbers(&lhs, &rhs))
        parse_error(tok, "Cannot compare except numbers");
      node = new_node_bop(ND_BITAND, lhs->expType, lhs, rhs);
    } else
      return node;
  }
}

static Node *xor(void) {
  Node *node = and();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_HAT)) != NULL) {
      Node *lhs = node, *rhs= and();
      if (!cast_numbers(&lhs, &rhs))
        parse_error(tok, "Cannot compare except numbers");
      node = new_node_bop(ND_BITXOR, lhs->expType, lhs, rhs);
    } else
      return node;
  }
}

static Node *or(void) {
  Node *node = xor();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_OR)) != NULL) {
      Node *lhs = node, *rhs= xor();
      if (!cast_numbers(&lhs, &rhs))
        parse_error(tok, "Cannot compare except numbers");
      node = new_node_bop(ND_BITOR, lhs->expType, lhs, rhs);
    } else
      return node;
  }
}

static Node *logand(void) {
  Node *node = or();
  for (;;) {
    if (consume(TK_LOGAND))
      node = new_node_bop(ND_LOGAND, &tyBool, node, or());
    else
      return node;
  }
}

static Node *logior(void) {
  Node *node = logand();
  for (;;) {
    if (consume(TK_LOGIOR))
      node = new_node_bop(ND_LOGIOR, &tyBool, node, logand());
    else
      return node;
  }
}

static Node *conditional(void) {
  Node *node = logior();
  for (;;) {
    if (!consume(TK_QUESTION))
      return node;
    Node *t = expr();
    if (!consume(TK_COLON))
      parse_error(NULL, "`:' expected");
    Node *f = conditional();
    if (!same_type(t->expType, f->expType))
      parse_error(NULL, "lhs and rhs must be same type");
    //node = new_node_ternary(t->expType, node, t, f);
    node = new_node_if(node, t, f, t->expType);
  }
}

static Node *assign(void) {
  Node *node = conditional();

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

static Node *expr(void) {
  return assign();
}

static Node *parse_if(void) {
  if (consume(TK_LPAR)) {
    Node *cond = expr();
    if (consume(TK_RPAR)) {
      Node *tblock = stmt();
      Node *fblock = NULL;
      if (consume(TK_ELSE)) {
        fblock = stmt();
      }
      return new_node_if(cond, tblock, fblock, &tyVoid);
    }
  }
  parse_error(NULL, "Illegal syntax in `if'");
  return NULL;
}

static Node *parse_switch(void) {
  if (consume(TK_LPAR)) {
    Node *value = expr();
    if (consume(TK_RPAR)) {
      Node *swtch = new_node_switch(value);

      Node *save_switch = curswitch;
      int save_flag = curloopflag;
      curloopflag |= LF_BREAK;
      curswitch = swtch;

      swtch->u.switch_.body = stmt();

      curloopflag = save_flag;
      curswitch = save_switch;

      return swtch;
    }
  }
  parse_error(NULL, "Illegal syntax in `switch'");
  return NULL;
}

static Node *parse_case(Token *tok) {
  if (curswitch == NULL)
    parse_error(tok, "`case' cannot use outside of `switch`");

  tok = fetch_token();
  Node *valnode = expr();
  intptr_t value;
  switch (valnode->type) {  // TODO: Accept const expression.
  case ND_CHAR:
  case ND_INT:
  case ND_LONG:
    value = valnode->u.value;
    break;
  default:
    parse_error(tok, "Cannot use expression");
    break;
  }
  if (!consume(TK_COLON))
    parse_error(NULL, "`:' expected");

  Vector *values = curswitch->u.switch_.case_values;
  if (values == NULL)
    curswitch->u.switch_.case_values = values = new_vector();

  // Check duplication.
  for (int i = 0, len = values->len; i < len; ++i) {
    if ((intptr_t)values->data[i] == value)
      parse_error(tok, "Case value `%lld' already defined: %s", value);
  }

  vec_push(values, (void*)value);

  return new_node_case(value);
}

static Node *parse_default(Token *tok) {
  if (curswitch == NULL)
    parse_error(tok, "`default' cannot use outside of `switch'");
  if (curswitch->u.switch_.has_default)
    parse_error(tok, "`default' already defined in `switch'");

  if (!consume(TK_COLON))
    parse_error(NULL, "`:' expected");

  curswitch->u.switch_.has_default = true;

  return new_node_default();
}

static Node *parse_while(void) {
  if (consume(TK_LPAR)) {
    Node *cond = expr();
    if (consume(TK_RPAR)) {
      int save_flag = curloopflag;
      curloopflag |= LF_BREAK | LF_CONTINUE;
      Node *body = stmt();
      curloopflag = save_flag;

      return new_node_while(cond, body);
    }
  }
  parse_error(NULL, "Illegal syntax in `while'");
  return NULL;
}

static Node *parse_do_while(void) {
  int save_flag = curloopflag;
  curloopflag |= LF_BREAK | LF_CONTINUE;
  Node *body = stmt();
  curloopflag = save_flag;

  if (consume(TK_WHILE)) {
    if (consume(TK_LPAR)) {
      Node *cond = expr();
      if (consume(TK_RPAR) && consume(TK_SEMICOL)) {
        return new_node_do_while(body, cond);
      }
    }
  }
  parse_error(NULL, "Illegal syntax in `do-while'");
  return NULL;
}

static Vector *parse_vardecl_cont(const Type *rawType, const Type *type, int flag, Token *ident);
static Node *parse_for(void) {
  Scope *scope = NULL;
  if (consume(TK_LPAR)) {
    assert(curfunc != NULL);
    Node *pre = NULL;
    bool nopre = false;
    Vector *stmts = NULL;
    if (consume(TK_SEMICOL)) {
      nopre = true;
    } else {
      const Type *rawType = NULL;
      const Type *type;
      int flag;
      Token *ident;
      if (parse_var_def(&rawType, &type, &flag, &ident, false)) {
        scope = enter_scope(curfunc, NULL);
        stmts = parse_vardecl_cont(rawType, type, flag, ident);
        if (!consume(TK_SEMICOL))
          scope = NULL;  // Error
      } else {
        pre = expr();
        if (!consume(TK_SEMICOL))
          pre = NULL;  // Error
      }
    }
    if (nopre || pre != NULL || scope != NULL) {
      Node *cond = NULL, *post = NULL;
      Node *body = NULL;
      if ((consume(TK_SEMICOL) || (cond = expr(), consume(TK_SEMICOL))) &&
          (consume(TK_RPAR) || (post = expr(), consume(TK_RPAR)))) {
        int save_flag = curloopflag;
        curloopflag |= LF_BREAK | LF_CONTINUE;
        body = stmt();
        curloopflag= save_flag;

        Node *node = new_node_for(pre, cond, post, body);
        if (stmts != NULL) {
          vec_push(stmts, node);
          exit_scope();
          return new_node_block(scope, stmts);
        } else {
          return node;
        }
      }
    }
  }
  if (scope != NULL)
    exit_scope();
  parse_error(NULL, "Illegal syntax in `for'");
  return NULL;
}

static Node *parse_break_continue(enum NodeType type) {
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");
  return new_node(type, &tyVoid);
}

static Node *parse_return(void) {
  assert(curfunc != NULL);

  Node *val = NULL;
  Token *tok;
  const Type *rettype = curfunc->type->u.func.ret;
  if ((tok = consume(TK_SEMICOL)) != NULL) {
    if (rettype->type != TY_VOID)
      parse_error(tok, "`return' required a value");
  } else {
    tok = fetch_token();
    val = expr();
    if (!consume(TK_SEMICOL))
      parse_error(NULL, "`;' expected");

    if (rettype->type == TY_VOID)
      parse_error(tok, "void function `return' a value");
    val = new_node_cast(rettype, val, false);
  }
  return new_node_return(val);
}

// Initializer

static Initializer *parse_initializer(void) {
  Initializer *result = malloc(sizeof(*result));
  if (consume(TK_LBRACE)) {
    Vector *multi = new_vector();
    if (!consume(TK_RBRACE)) {
      for (;;) {
        Initializer *init;
        if (consume(TK_DOT)) {  // .member=value
          Token *ident = consume(TK_IDENT);
          if (ident == NULL)
            parse_error(NULL, "`ident' expected for dotted initializer");
          if (!consume(TK_ASSIGN))
            parse_error(NULL, "`=' expected for dotted initializer");
          Initializer *value = parse_initializer();
          init = malloc(sizeof(*init));
          init->type = vDot;
          init->u.dot.name = ident->u.ident;
          init->u.dot.value = value;
        } else {
          init = parse_initializer();
        }
        vec_push(multi, init);

        if (consume(TK_COMMA)) {
          if (consume(TK_RBRACE))
            break;
        } else {
          if (!consume(TK_RBRACE))
            parse_error(NULL, "`}' or `,' expected");
          break;
        }
      }
    }
    result->type = vMulti;
    result->u.multi = multi;
  } else {
    result->type = vSingle;
    result->u.single = expr();
  }
  return result;
}

static Vector *clear_initial_value(Node *node, Vector *inits) {
  if (inits == NULL)
    inits = new_vector();

  switch (node->expType->type) {
  case TY_CHAR:
  case TY_INT:
  case TY_LONG:
  case TY_ENUM:
    vec_push(inits,
             new_node_bop(ND_ASSIGN, node->expType, node,
                          new_node_cast(node->expType, new_node_numlit(ND_INT, 0), true)));
    break;
  case TY_PTR:
    vec_push(inits,
             new_node_bop(ND_ASSIGN, node->expType, node,
                          new_node_cast(node->expType, new_node_numlit(ND_LONG, 0), true)));  // intptr_t
    break;
  case TY_ARRAY:
    {
      size_t arr_len = node->expType->u.pa.length;
      for (size_t i = 0; i < arr_len; ++i)
        clear_initial_value(new_node_deref(add_node(NULL, node, new_node_numlit(ND_INT, i))), inits);
    }
    break;
  case TY_STRUCT:
    {
      const StructInfo *sinfo = node->expType->u.struct_.info;
      assert(sinfo != NULL);
      for (int i = 0; i < sinfo->members->len; ++i) {
        VarInfo* varinfo = sinfo->members->data[i];
        Node *member = new_node_member(node, varinfo->name, varinfo->type);
        clear_initial_value(member, inits);
      }
    }
    break;
  case TY_UNION:
  default:
    assert(!"Not implemented");
    break;
  }

  return inits;
}

static void string_initializer(Node *dst, Node *src, Vector *inits) {
  // Initialize char[] with string literal (char s[] = "foo";).
  assert(dst->expType->type == TY_ARRAY && dst->expType->u.pa.ptrof->type == TY_CHAR);
  assert(src->expType->type == TY_ARRAY && src->expType->u.pa.ptrof->type == TY_CHAR);

  const char *str = src->u.str.buf;
  size_t len = src->u.str.len;
  size_t dstlen = dst->expType->u.pa.length;
  if (dstlen == (size_t)-1) {
    ((Type*)dst->expType)->u.pa.length = dstlen = len;
  } else {
    if (dstlen < len)
      parse_error(NULL, "Buffer is shorter than string: %d for \"%s\"", (int)dstlen, str);
  }

  for (size_t i = 0; i < len; ++i) {
    Node *index = new_node_numlit(ND_INT, i);
    vec_push(inits,
             new_node_bop(ND_ASSIGN, &tyChar,
                          new_node_deref(add_node(NULL, dst, index)),
                          new_node_deref(add_node(NULL, src, index))));
  }
  if (dstlen > len) {
    Node *zero = new_node_numlit(ND_CHAR, 0);
    for (size_t i = len; i < dstlen; ++i) {
      Node *index = new_node_numlit(ND_INT, i);
      vec_push(inits,
               new_node_bop(ND_ASSIGN, &tyChar,
                            new_node_deref(add_node(NULL, dst, index)),
                            zero));
    }
  }
}

static Vector *assign_initial_value(Node *node, Initializer *init, Vector *inits) {
  if (inits == NULL)
    inits = new_vector();

  switch (node->expType->type) {
  case TY_ARRAY:
    {
      // Special handling for string (char[]).
      if (node->expType->u.pa.ptrof->type == TY_CHAR &&
          init->type == vSingle &&
          can_cast(node->expType, init->u.single->expType, init->u.single, false)) {
        string_initializer(node, init->u.single, inits);
        break;
      }

      if (init->type != vMulti)
        parse_error(NULL, "Error initializer");
      size_t arr_len = node->expType->u.pa.length;
      if (arr_len == (size_t)-1) {
        ((Type*)node->expType)->u.pa.length = arr_len = init->u.multi->len;
      } else {
        if ((size_t)init->u.multi->len > arr_len)
          parse_error(NULL, "Initializer more than array size");
      }
      int len = init->u.multi->len;
      for (int i = 0; i < len; ++i) {
        assign_initial_value(new_node_deref(add_node(NULL, node, new_node_numlit(ND_INT, i))),
                             init->u.multi->data[i], inits);
      }
      // Clear left.
      for (size_t i = len; i < arr_len; ++i)
        clear_initial_value(new_node_deref(add_node(NULL, node, new_node_numlit(ND_INT, i))), inits);
    }
    break;
  case TY_STRUCT:
    {
      if (init->type != vMulti)
        parse_error(NULL, "`{...}' expected for initializer");

      ensure_struct((Type*)node->expType, NULL);
      const StructInfo *sinfo = node->expType->u.struct_.info;
      int n = sinfo->members->len;
      int m = init->u.multi->len;
      if (n <= 0) {
        if (m > 0)
          parse_error(NULL, "Initializer for empty struct");
        break;
      }
      Initializer **values = malloc(sizeof(Initializer*) * n);
      for (int i = 0; i < n; ++i)
        values[i] = NULL;

      int dst = -1;
      for (int i = 0; i < m; ++i) {
        Initializer *value = init->u.multi->data[i];
        if (value->type == vDot) {
          int idx = var_find(sinfo->members, value->u.dot.name);
          if (idx < 0)
            parse_error(NULL, "`%s' is not member of struct", value->u.dot.name);
          values[idx] = value->u.dot.value;
          dst = idx;
          continue;
        }
        if (++dst >= n)
          break;  // TODO: Check extra.
        values[dst] = value;
      }
      for (int i = 0; i < n; ++i) {
        VarInfo* varinfo = sinfo->members->data[i];
        Node *member = new_node_member(node, varinfo->name, varinfo->type);
        if (values[i] != NULL)
          assign_initial_value(member, values[i], inits);
        else
          clear_initial_value(member, inits);
      }
    }
    break;
  case TY_UNION:
    {
      if (init->type != vMulti)
        parse_error(NULL, "`{...}' expected for initializer");

      const StructInfo *sinfo = node->expType->u.struct_.info;
      ensure_struct((Type*)node->expType, NULL);
      int n = sinfo->members->len;
      int m = init->u.multi->len;
      if (n <= 0 && m > 0)
        parse_error(NULL, "Initializer for empty union");

      int dst = 0;
      Initializer *value = init->u.multi->data[0];
      if (value->type == vDot) {
        int idx = var_find(sinfo->members, value->u.dot.name);
        if (idx < 0)
          parse_error(NULL, "`%s' is not member of struct", value->u.dot.name);
        dst = idx;
        value = value->u.dot.value;
      }
      VarInfo* varinfo = sinfo->members->data[dst];
      Node *member = new_node_member(node, varinfo->name, varinfo->type);
      assign_initial_value(member, value, inits);
    }
    break;
  default:
    if (init->type != vSingle)
      parse_error(NULL, "Error initializer");
    vec_push(inits,
             new_node_bop(ND_ASSIGN, node->expType, node, new_node_cast(node->expType, init->u.single, false)));
    break;
  }

  return inits;
}

static Vector *parse_vardecl_cont(const Type *rawType, const Type *type, int flag, Token *ident) {
  Vector *inits = NULL;
  bool first = true;
  do {
    if (!first) {
      if (!parse_var_def(&rawType, &type, &flag, &ident, false)) {
        parse_error(NULL, "`ident' expected");
        return NULL;
      }
    }
    first = false;
    not_void(type);

    scope_add(curscope, ident, type, flag);

    if (consume(TK_ASSIGN)) {
      Initializer *initializer = parse_initializer();
      inits = assign_initial_value(new_node_varref(ident->u.ident, type, false), initializer, inits);
    }
  } while (consume(TK_COMMA));

  return inits;
}

static bool parse_vardecl(Node **pnode) {
  assert(curfunc != NULL);

  const Type *rawType = NULL;
  const Type *type;
  int flag;
  Token *ident;
  if (!parse_var_def(&rawType, &type, &flag, &ident, false))
    return false;

  Vector *inits = parse_vardecl_cont(rawType, type, flag, ident);

  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");

  if (inits != NULL && inits->len == 1)
    *pnode = inits->data[0];
  else
    *pnode = new_node_block(NULL, inits);
  return true;
}

// Multiple stmt-s, also accept `case` and `default`.
static Vector *read_stmts(void) {
  Vector *nodes = NULL;
  for (;;) {
    if (consume(TK_RBRACE))
      return nodes;
    if (nodes == NULL)
      nodes = new_vector();

    Node *node;
    Token *tok;
    if (parse_vardecl(&node))
      ;
    else if ((tok = consume(TK_CASE)) != NULL)
      node = parse_case(tok);
    else if ((tok = consume(TK_DEFAULT)) != NULL)
      node = parse_default(tok);
    else
      node = stmt();
    vec_push(nodes, node);
  }
}

static Node *parse_block(void) {
  assert(curfunc != NULL);
  Scope *scope = enter_scope(curfunc, NULL);
  Vector *nodes = read_stmts();
  exit_scope();
  return new_node_block(scope, nodes);
}

static Node *stmt(void) {
  if (consume(TK_SEMICOL))
    return new_node_block(NULL, NULL);

  if (consume(TK_LBRACE))
    return parse_block();

  if (consume(TK_IF))
    return parse_if();

  if (consume(TK_SWITCH))
    return parse_switch();

  if (consume(TK_WHILE))
    return parse_while();

  if (consume(TK_DO))
    return parse_do_while();

  if (consume(TK_FOR))
    return parse_for();

  Token *tok;
  if ((tok = consume(TK_BREAK)) != NULL) {
    if ((curloopflag & LF_BREAK) == 0)
      parse_error(tok, "`break' cannot be used outside of loop");
    return parse_break_continue(ND_BREAK);
  }
  if ((tok = consume(TK_CONTINUE)) != NULL) {
    if ((curloopflag & LF_CONTINUE) == 0)
      parse_error(tok, "`continue' cannot be used outside of loop");
    return parse_break_continue(ND_CONTINUE);
  }

  if (consume(TK_RETURN))
    return parse_return();

  // expression statement.
  Node *node = expr();
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "Semicolon required");
  return node;
}

static Vector *funparams(void) {
  Vector *params = NULL;
  if (consume(TK_RPAR)) {
    // Arbitrary funparams.
  } else {
    params = new_vector();
    for (;;) {
      const Type *type;
      int flag;
      Token *ident;
      if (!parse_var_def(NULL, &type, &flag, &ident, true))
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
      parse_error(NULL, "Comma or `}' expected");
    }
  }
  return params;
}

static Node *parse_defun(const Type *rettype, int flag, Token *ident) {
  const char *name = ident->u.ident;
  Vector *params = funparams();

  const Type *functype = new_func_type(rettype, params);

  Defun *defun = NULL;
  if (consume(TK_SEMICOL)) {  // Prototype declaration.
  } else {
    if (!consume(TK_LBRACE)) {
      parse_error(NULL, "`;' or `{' expected");
      return NULL;
    }
    // Definition.
    defun = new_defun(functype, name);
  }

  GlobalVarInfo *def = find_global(name);
  if (def == NULL) {
    define_global(functype, flag | VF_CONST, ident, NULL);
  } else {
    if (def->type->type != TY_FUNC)
      parse_error(ident, "Definition conflict: `%s'");
    // TODO: Check type.
    // TODO: Check duplicated definition.
    if (def->init != NULL)
      parse_error(ident, "`%s' function already defined");
  }

  if (defun != NULL) {
    curfunc = defun;

    enter_scope(defun, params);  // Scope for parameters.
    defun->top_scope = enter_scope(defun, NULL);
    defun->stmts = read_stmts();
    exit_scope();
    exit_scope();
    curfunc = NULL;
  }
  return defun != NULL ? new_node_defun(defun) : NULL;
}

static void parse_typedef(void) {
  int flag;
  Token *ident;
  const Type *type = parse_full_type(&flag, &ident);
  if (type == NULL)
    parse_error(NULL, "type expected");
  not_void(type);

  if (ident == NULL) {
    ident = consume(TK_IDENT);
    if (ident == NULL)
      parse_error(NULL, "ident expected");
  }
  const char *name = ident->u.ident;

  map_put(typedef_map, name, type);

  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");
}

static Node *define_global_var(const Type *rawtype, int flag, const Type *type, Token *ident) {
  bool first = true;
  for (;;) {
    if (!first) {
      type = parse_type_modifier(rawtype);
      if ((ident = consume(TK_IDENT)) == NULL)
        parse_error(NULL, "`ident' expected");
    }
    first = false;

    if (type->type == TY_VOID)
      parse_error(ident, "`void' not allowed");

    type = parse_type_suffix(type);
    Initializer *initializer = NULL;
    if (consume(TK_ASSIGN)) {
      if (flag & VF_EXTERN)
        parse_error(NULL, "extern with initializer");
      initializer = parse_initializer();
    }
    define_global(type, flag, ident, initializer);

    if (consume(TK_COMMA))
      continue;
    if (!consume(TK_SEMICOL))
      parse_error(NULL, "`;' or `,' expected");
    break;
  }
  return NULL;
}

static Node *toplevel(void) {
  int flag;
  const Type *rawtype = parse_raw_type(&flag);
  if (rawtype != NULL) {
    const Type *type = parse_type_modifier(rawtype);
    if ((is_struct_or_union(type->type) || type->type == TY_ENUM) &&
        consume(TK_SEMICOL))  // Just struct/union definition.
      return NULL;

    Token *ident;
    if ((ident = consume(TK_IDENT)) != NULL) {
      if (consume(TK_LPAR))  // Function.
        return parse_defun(type, flag, ident);

      define_global_var(rawtype, flag, type, ident);
      return NULL;
    }
    parse_error(NULL, "ident expected");
    return NULL;
  }
  if (consume(TK_TYPEDEF)) {
    parse_typedef();
    return NULL;
  }
  parse_error(NULL, "Unexpected token");
  return NULL;
}

Vector *parse_program(void) {
  Vector *node_vector = new_vector();
  while (!consume(TK_EOF)) {
    Node *node = toplevel();
    if (node != NULL)
      vec_push(node_vector, node);
  }
  return node_vector;
}
