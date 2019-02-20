#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "xcc.h"

static const Type tyVoid = {.type=TY_VOID, .ptrof=NULL};
static const Type tyInt = {.type=TY_INT, .ptrof=NULL};
static const Type tyChar = {.type=TY_CHAR, .ptrof=NULL};
static const Type tyLong = {.type=TY_LONG, .ptrof=NULL};
static const Type tyStr = {.type=TY_PTR, .ptrof=&tyChar};
#define tyBool  tyInt

static StructInfo *parse_struct(void);
static Node *stmt(void);
static Node *expr(void);

//

int var_find(Vector *lvars, const char *name) {
  for (int i = 0, len = lvars->len; i < len; ++i) {
    VarInfo *info = (VarInfo*)lvars->data[i];
    if (strcmp(info->name, name) == 0)
      return i;
  }
  return -1;
}

void var_add(Vector *lvars, const char *name, const Type *type) {
  int idx = var_find(lvars, name);
  if (idx >= 0)
    error("`%s' already defined", name);

  VarInfo *info = malloc(sizeof(*info));
  info->name = name;
  info->type = type;
  info->offset = -1;
  vec_push(lvars, info);
}

// Struct

Map *struct_map;

// Global

Map *global;

VarInfo *find_global(const char *name) {
  return (VarInfo*)map_get(global, name);
}

void define_global(const Type *type, const char *name) {
  VarInfo *varinfo = find_global(name);
  if (varinfo != NULL)
    error("`%s' already defined", name);
  varinfo = malloc(sizeof(*varinfo));
  varinfo->name = name;
  varinfo->type = type;
  varinfo->offset = 0;
  map_put(global, name, varinfo);
}

// Type

void dump_type(FILE *fp, const Type *type) {
  switch (type->type) {
  case TY_VOID: fprintf(fp, "void"); break;
  case TY_CHAR: fprintf(fp, "char"); break;
  case TY_INT: fprintf(fp, "int"); break;
  case TY_LONG: fprintf(fp, "long"); break;
  case TY_PTR: dump_type(fp, type->ptrof); fprintf(fp, "*"); break;
  case TY_ARRAY: dump_type(fp, type->ptrof); fprintf(fp, "[%d]", (int)type->array_size); break;
  default: assert(false);
  }
}

static bool is_number(enum eType type) {
  return type == TY_INT || type == TY_CHAR;
}

static bool same_type(const Type *type1, const Type *type2) {
  for (;;) {
    if (type1->type != type2->type)
      return false;

    switch (type1->type) {
    case TY_VOID:
    case TY_CHAR:
    case TY_INT:
    case TY_LONG:
      return true;
    case TY_PTR:
    case TY_ARRAY:
      type1 = type1->ptrof;
      type2 = type2->ptrof;
      continue;
    case TY_FUNC:
      return type1 == type2;
    case TY_STRUCT:
      return type1->struct_ == type2->struct_;
    }
  }
}

static Type* ptrof(const Type *type) {
  Type *ptr = malloc(sizeof(*ptr));
  ptr->type = TY_PTR;
  ptr->ptrof = type;
  return ptr;
}

static const Type *array_to_ptr(const Type *type) {
  if (type->type != TY_ARRAY)
    return type;
  return ptrof(type->ptrof);
}

static Type* arrayof(const Type *type, size_t array_size) {
  Type *arr = malloc(sizeof(*arr));
  arr->type = TY_ARRAY;
  arr->ptrof = type;
  arr->array_size = array_size;
  return arr;
}

static Type* new_func_type(const Type *ret, const Vector *params) {
  Type *f = malloc(sizeof(*f));
  f->type = TY_FUNC;
  f->func.ret = ret;

  Vector *newparams = NULL;
  if (params != NULL) {
    // Clone params.
    newparams = new_vector();
    for (int i = 0; i < params->len; ++i)
      vec_push(newparams, params->data[i]);
  }
  f->func.params = newparams;
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

static void scope_add(Scope *scope, const char *name, const Type *type) {
  if (scope->vars == NULL)
    scope->vars = new_vector();
  var_add(scope->vars, name, type);
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

static Defun *new_defun(const Type *rettype, const char *name, Vector *params) {
  Defun *defun = malloc(sizeof(*defun));
  defun->rettype = rettype;
  defun->name = name;
  defun->stmts = NULL;
  defun->all_scopes = new_vector();
  defun->ret_label = NULL;
  defun->top_scope = enter_scope(defun, params);

  return defun;
}

//

const int LF_BREAK = 1 << 0;
const int LF_CONTINUE = 1 << 0;

static Defun *curfunc;
static int curloopflag;

static Node *new_node(enum NodeType type, const Type *expType) {
  Node *node = malloc(sizeof(Node));
  node->type = type;
  node->expType = expType;
  return node;
}

static Node *new_node_cast(const Type *type, Node *sub, bool is_explicit) {
  if (same_type(type, sub->expType))
    return sub;

  if (type->type == TY_VOID || sub->expType->type == TY_VOID)
    error("cannot use `void' as a value");

  switch (type->type) {
  case TY_CHAR:
    switch (sub->expType->type) {
    case TY_INT:  goto ok;  // TODO: Raise warning if implicit.
    case TY_LONG:  goto ok;  // TODO: Raise warning if implicit.
    default:  break;
    }
    break;
  case TY_INT:
    switch (sub->expType->type) {
    case TY_CHAR:  goto ok;
    case TY_LONG:  goto ok;
    default:  break;
    }
    break;
  case TY_LONG:
    switch (sub->expType->type) {
    case TY_CHAR:  goto ok;
    case TY_INT:  goto ok;
    case TY_PTR:
      if (is_explicit) {
        // TODO: Check sizeof(long) is same as sizeof(ptr)
        goto ok;
      }
      break;
    default:  break;
    }
    break;
  case TY_PTR:
    switch (sub->expType->type) {
    case TY_INT:
    case TY_LONG:
      if (is_explicit)
        goto ok;
      break;
    case TY_PTR:
      if (is_explicit)
        goto ok;
      // void* is interchangable with any pointer type.
      if (type->ptrof->type == TY_VOID || sub->expType->ptrof->type == TY_VOID)
        goto ok;
      break;
    case TY_ARRAY:
      if (is_explicit)
        goto ok;
      if (same_type(type->ptrof, sub->expType->ptrof))
        goto ok;
      break;
    default:  break;
    }
    break;
  default:
    break;
  }
  error("Cannot convert value from type %d to %d: %s", sub->expType->type, type->type, current_line());
  return NULL;

 ok:;
  Node *node = new_node(ND_CAST, type);
  node->cast.sub = sub;
  return node;
}

static Node *new_node_bop(enum NodeType type, const Type *expType, Node *lhs, Node *rhs) {
  Node *node = new_node(type, expType);
  node->bop.lhs = lhs;
  node->bop.rhs = rhs;
  return node;
}

static Node *new_node_unary(enum NodeType type, const Type *expType, Node *sub) {
  Node *node = new_node(type, expType);
  node->unary.sub = sub;
  return node;
}

static Node *new_node_deref(Node *sub) {
  if (sub->expType->type != TY_PTR && sub->expType->type != TY_ARRAY)
    error("Cannot dereference raw type");
  return new_node_unary(ND_DEREF, sub->expType->ptrof, sub);
}

static Node *new_node_intlit(int val) {
  Node *node = new_node(ND_INT, &tyInt);
  node->intval = val;
  return node;
}

static Node *new_node_charlit(int val) {
  Node *node = new_node(ND_CHAR, &tyChar);
  node->charval = val;
  return node;
}

static Node *new_node_longlit(long val) {
  Node *node = new_node(ND_LONG, &tyLong);
  node->longval = val;
  return node;
}

static Node *new_node_str(const char *str) {
  Node *node = new_node(ND_STR, &tyStr);
  node->str = str;
  return node;
}

static Node *new_node_varref(const char *name, const Type *type, int global) {
  Node *node = new_node(ND_VARREF, type);
  node->varref.ident = name;
  node->varref.global = global;
  return node;
}

static Node *new_node_member(Node *target, const char *name, const Type *expType) {
  Node *node = new_node(ND_MEMBER, expType);
  node->member.target = target;
  node->member.name = name;
  return node;
}

static Node *new_node_defun(const Type *rettype, const char *name, Vector *params) {
  Node *node = new_node(ND_DEFUN, &tyVoid);
  node->defun = new_defun(rettype, name, params);
  return node;
}

static Node *new_node_funcall(Node *func, Vector *args) {
  const Type *rettype = func->expType->type == TY_FUNC ? func->expType->func.ret : &tyInt;  // TODO: Fix.
  Node *node = new_node(ND_FUNCALL, rettype);
  node->funcall.func = func;
  node->funcall.args = args;
  return node;
}

static Node *new_node_block(Scope *scope, Vector *nodes) {
  Node *node = new_node(ND_BLOCK, &tyVoid);
  node->block.scope = scope;
  node->block.nodes = nodes;
  return node;
}

static Node *new_node_if(Node *cond, Node *tblock, Node *fblock) {
  Node *node = new_node(ND_IF, &tyVoid);
  node->if_.cond = cond;
  node->if_.tblock = tblock;
  node->if_.fblock = fblock;
  return node;
}

static Node *new_node_while(Node *cond, Node *body) {
  Node *node = new_node(ND_WHILE, &tyVoid);
  node->while_.cond = cond;
  node->while_.body = body;
  return node;
}

static Node *new_node_do_while(Node *body, Node *cond) {
  Node *node = new_node(ND_DO_WHILE, &tyVoid);
  node->do_while.body = body;
  node->do_while.cond = cond;
  return node;
}

static Node *new_node_for(Node *pre, Node *cond, Node *post, Node *body) {
  Node *node = new_node(ND_FOR, &tyVoid);
  node->for_.pre = pre;
  node->for_.cond = cond;
  node->for_.post = post;
  node->for_.body = body;
  return node;
}

static Node *new_node_return(Node *val) {
  const Type *type = val != NULL ? val->expType : &tyVoid;
  Node *node = new_node(ND_RETURN, type);
  node->return_.val = val;
  return node;
}

static Node *funcall(Node *func) {
  if (!(func->expType->type == TY_FUNC ||
        func->expType->type == TY_PTR))  // TODO: Restrict to function pointer.
    error("Cannot call except funtion");

  const Type *functype = func->expType->type == TY_FUNC ? func->expType : NULL;

  Vector *args = NULL;
  if (!consume(TK_RPAR)) {
    args = new_vector();
    for (;;) {
      Node *arg = expr();
      if (functype != NULL && functype->func.params != NULL && args->len < functype->func.params->len) {
        const Type * type = ((VarInfo*)functype->func.params->data[args->len])->type;
        arg = new_node_cast(type, arg, false);
      }
      vec_push(args, arg);
      if (consume(TK_RPAR))
        break;
      if (consume(TK_COMMA))
        continue;
      error("Comma or `)` expected, but %s", current_line());
    }
  }

  if (functype != NULL && functype->func.params != NULL && (args != NULL ? args->len : 0) != functype->func.params->len)
    error("function `%s' expect %d arguments, but %d\n", func->varref.ident, functype->func.params->len, (args != NULL ? args->len : 0));

  return new_node_funcall(func, args);
}

static Node *add_node(Node *lhs, Node *rhs) {
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
      return new_node_bop(ND_ADD, l->expType, new_node_cast(r->expType, l, false), r);
    default:
      break;
    }
    break;

  case TY_INT:
    switch (r->expType->type) {
    case TY_INT:
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

  error("Illegal `+' at %s", current_line());
  return NULL;
}

static Node *sub_node(Node *lhs, Node *rhs) {
  switch (lhs->expType->type) {
  case TY_INT:
    switch (rhs->expType->type) {
    case TY_INT:
    case TY_CHAR:
      return new_node_bop(ND_SUB, lhs->expType, lhs, new_node_cast(lhs->expType, rhs, false));
    default:
      break;
    }
    break;

  case TY_CHAR:
    switch (rhs->expType->type) {
    case TY_CHAR:
    case TY_INT:
      return new_node_bop(ND_SUB, rhs->expType, new_node_cast(rhs->expType, lhs, false), rhs);
    default:
      break;
    }
    break;

  case TY_PTR:
    switch (rhs->expType->type) {
    case TY_INT:
      return new_node_bop(ND_PTRSUB, lhs->expType, lhs, rhs);
    case TY_PTR:
      if (!same_type(lhs->expType, rhs->expType))
        error("Different pointer sub");
      return new_node_bop(ND_PTRDIFF, &tyInt, lhs, rhs);  // TODO: size_t
    case TY_ARRAY:
      if (!same_type(lhs->expType->ptrof, rhs->expType->ptrof))
        error("Different pointer sub");
      return new_node_bop(ND_PTRDIFF, &tyInt, lhs, rhs);  // TODO: size_t
    default:
      break;
    }
    break;

  case TY_ARRAY:
    switch (rhs->expType->type) {
    case TY_PTR:
      if (!same_type(lhs->expType->ptrof, rhs->expType->ptrof))
        error("Different pointer sub");
      return new_node_bop(ND_PTRDIFF, &tyInt, lhs, rhs);  // TODO: size_t
    case TY_ARRAY:
      if (!same_type(lhs->expType, rhs->expType))
        error("Different pointer sub");
      return new_node_bop(ND_PTRDIFF, &tyInt, lhs, rhs);  // TODO: size_t
    default:
      break;
    }
    break;

  default:
    break;
  }

  error("Illegal `-' at %s", current_line());
  return NULL;
}

static Node *arith_node(enum NodeType nodeType, Node *lhs, Node *rhs) {
  if (!is_number(lhs->expType->type) || !is_number(rhs->expType->type))
    error("Cannot use `%d' except numbers.", nodeType);

  const Type *expType = lhs->expType;
  if (rhs->expType->type > expType->type)
    expType = rhs->expType;

  return new_node_bop(nodeType, expType, lhs, rhs);
}

Node *array_index(Node *array) {
  Node *index = expr();
  if (!consume(TK_RBRACKET))
    error("`]' expected, but %s", current_line());
  return new_node_deref(add_node(array, index));
}

Node *member_access(Node *target, enum TokenType toktype) {
  Token *tok;
  if (!(tok = consume(TK_IDENT)))
    error("`ident' expected, but %s", current_line());
  const char *name = tok->ident;

  // Find member's type from struct info.
  const Type *type = target->expType;
  if (toktype == TK_DOT) {
    if (type->type != TY_STRUCT)
      error("`.' for non struct value");
  } else {  // TK_ARROW
    if (type->type != TY_PTR)
      error("`->' for non pointer value");
    type = target->expType->ptrof;
    if (type->type != TY_STRUCT)
      error("`->' for non struct value");
  }

  int index = var_find(type->struct_->members, name);
  if (index < 0)
    error("`%s' doesn't exist in the struct");
  VarInfo *varinfo = (VarInfo*)type->struct_->members->data[index];

  return new_node_member(target, name, varinfo->type);
}

static const Type *parse_raw_type(void) {
  Type *type = NULL;

  if (consume(TK_STRUCT)) {
    const char *name = NULL;
    Token *tok;
    if ((tok = consume(TK_IDENT)))
      name = tok->ident;

    StructInfo *sinfo;
    if (consume(TK_LBRACE)) {  // Definition
      sinfo = parse_struct();
      if (name != NULL)
        map_put(struct_map, name, sinfo);  // TODO: Already defined?
    } else {
      sinfo = (StructInfo*)map_get(struct_map, name);
      if (sinfo == NULL)
        error("Undefined struct: %s", name);
    }
    type = malloc(sizeof(*type));
    type->type = TY_STRUCT;
    type->struct_ = sinfo;
  } else {
    static const enum TokenType kKeywords[] = {
      TK_KWVOID, TK_KWCHAR, TK_KWINT, TK_KWLONG,
    };
    static const enum eType kTypes[] = {
      TY_VOID, TY_CHAR, TY_INT, TY_LONG,
    };
    const int N = sizeof(kTypes) / sizeof(*kTypes);
    for (int i = 0; i < N; ++i) {
      if (consume(kKeywords[i])) {
        type = malloc(sizeof(*type));
        type->type = kTypes[i];
        type->ptrof = NULL;
        break;
      }
    }
  }
  return type;
}

static const Type *parse_type_modifier(const Type* type, bool allow_void) {
  if (type == NULL)
    return NULL;

  while (consume(TK_MUL))
    type = ptrof(type);

  if (!allow_void && type->type == TY_VOID)
    error("`void' not allowed");
  return type;
}

static const Type *parse_var_def_suffix(const Type *type) {
  if (!consume(TK_LBRACKET))
    return type;
  Token *tok;
  int count;
  if (consume(TK_RBRACKET)) {
    count = -1;
  } else if ((tok = consume(TK_INTLIT))) {  // TODO: Constant expression.
    count = tok->intval;
    if (count < 0)
      error("Array size must be greater than 0, but %d", count);
    if (!consume(TK_RBRACKET))
      error("`]' expected, but %s", current_line());
  } else {
    error("syntax error: %s", current_line());
  }
  return arrayof(parse_var_def_suffix(type), count);
}

static bool parse_var_def(const Type **prawType, const Type** ptype, const char **pname, bool allow_void) {
  if (*prawType == NULL) {
    const Type *rawType = parse_raw_type();
    if (rawType == NULL)
      return false;
    *prawType = rawType;
  }

  const Type *type = parse_type_modifier(*prawType, allow_void);

  Token *tok;
  const char *name = NULL;
  if (!allow_void || type->type != TY_VOID) {
    if (!(tok = consume(TK_IDENT)))
      error("Ident expected, but %s", current_line());
    name = tok->ident;
    type = parse_var_def_suffix(type);
  }

  *ptype = type;
  *pname = name;

  return true;
}

static StructInfo *parse_struct(void) {
  Vector *members = new_vector();
  for (;;) {
    if (consume(TK_RBRACE))
      break;

    const Type *rawType = NULL;
    const Type *type;
    const char *name;
    if (!parse_var_def(&rawType, &type, &name, false))
      error("type expected, but %s", current_line());

    if (!consume(TK_SEMICOL))
      error("semicolon expected, but %s", current_line());
    var_add(members, name, type);
  }

  StructInfo *sinfo = malloc(sizeof(*sinfo));
  sinfo->members = members;
  sinfo->size = 0;
  sinfo->align = 0;
  return sinfo;
}

static Node *prim(void) {
  if (consume(TK_LPAR)) {
    const Type *type = parse_raw_type();
    if (type != NULL) {  // Cast
      type = parse_type_modifier(type, true);
      if (!consume(TK_RPAR))
        error("`)' expected, but %s", current_line());
      Node *node = prim();
      return new_node_cast(type, node, true);
    } else {
      Node *node = expr();
      if (!consume(TK_RPAR))
        error("No close paren: %s", current_line());
      return node;
    }
  }

  Token *tok;
  if ((tok = consume(TK_INTLIT)))
    return new_node_intlit(tok->intval);
  if ((tok = consume(TK_CHARLIT)))
    return new_node_charlit(tok->charval);
  if ((tok = consume(TK_LONGLIT)))
    return new_node_longlit(tok->longval);
  if ((tok = consume(TK_STR)))
    return new_node_str(tok->str);

  if ((tok = consume(TK_IDENT))) {
    if (curfunc == NULL) {
      error("Cannot use variable outside of function: `%s'", tok->ident);
    } else {
      const Type *type = NULL;
      VarInfo *varinfo = scope_find(curscope, tok->ident);
      if (varinfo != NULL) {
        type = varinfo->type;
      } else {
        VarInfo *varinfo = find_global(tok->ident);
        if (varinfo == NULL)
          error("Undefined `%s'", tok->ident);
        type = varinfo->type;
      }
      int global = varinfo == NULL;
      return new_node_varref(tok->ident, type, global);
    }
  } else {
    error("Number or Ident or open paren expected: %s", current_line());
  }
  return NULL;
}

static Node *term(void) {
  if (consume(TK_ADD)) {
    Node *node = term();
    if (!is_number(node->expType->type))
      error("Cannot apply `+' except number types");
    return node;
  }

  if (consume(TK_SUB)) {
    Node *node = term();
    if (!is_number(node->expType->type))
      error("Cannot apply `-' except number types");
    if (node->type == ND_INT) {
      node->intval = -node->intval;
      return node;
    }
    return new_node_unary(ND_NEG, node->expType, node);
  }

  if (consume(TK_NOT)) {
    Node *node = term();
    switch (node->expType->type) {
    case TY_INT:
    case TY_CHAR:
    case TY_PTR:
      node = new_node_unary(ND_NOT, &tyBool, node);
      break;
    default:
      error("Cannot apply `!' except number or pointer types");
      break;
    }
    return node;
  }

  if (consume(TK_AMP)) {
    Node *node = term();
    return new_node_unary(ND_REF, ptrof(node->expType), node);
  }

  if (consume(TK_MUL)) {
    Node *node = term();
    return new_node_deref(node);
  }

  if (consume(TK_INC)) {
    Node *node = term();
    return new_node_unary(ND_PREINC, node->expType, node);
  }

  if (consume(TK_DEC)) {
    Node *node = term();
    return new_node_unary(ND_PREDEC, node->expType, node);
  }

  Node *node = prim();

  for (;;) {
    if (consume(TK_LPAR))
      node = funcall(node);
    else if (consume(TK_LBRACKET))
      node = array_index(node);
    else if (consume(TK_DOT))
      node = member_access(node, TK_DOT);
    else if (consume(TK_ARROW))
      node = member_access(node, TK_ARROW);
    else if (consume(TK_INC))
      node = new_node_unary(ND_POSTINC, node->expType, node);
    else if (consume(TK_DEC))
      node = new_node_unary(ND_POSTDEC, node->expType, node);
    else
      return node;
  }
}

static Node *mul(void) {
  Node *node = term();

  for (;;) {
    enum TokenType tt;
    enum NodeType t;
    if (consume(tt = TK_MUL))
      t = ND_MUL;
    else if (consume(tt = TK_DIV))
      t = ND_DIV;
    else if (consume(tt = TK_MOD))
      t = ND_MOD;
    else
      return node;

    return arith_node(t, node, term());
  }
}

static Node *add(void) {
  Node *node = mul();

  for (;;) {
    if (consume(TK_ADD))
      node = add_node(node, mul());
    else if (consume(TK_SUB))
      node = sub_node(node, mul());
    else
      return node;
  }
}

static Node *cmp(void) {
  Node *node = add();

  for (;;) {
    enum NodeType t;
    if (consume(TK_LT))
      t = ND_LT;
    else if (consume(TK_GT))
      t = ND_GT;
    else if (consume(TK_LE))
      t = ND_LE;
    else if (consume(TK_GE))
      t = ND_GE;
    else
      return node;

    Node *lhs = node, *rhs= add();
    if (lhs->expType->type == TY_PTR || rhs->expType->type == TY_PTR) {
      if (lhs->expType->type != TY_PTR || rhs->expType->type != TY_PTR ||
          !same_type(lhs->expType, rhs->expType))
        error("Cannot compare pointer to other types");
    } else {
      if (!is_number(lhs->expType->type) || !is_number(rhs->expType->type))
        error("Cannot compare except numbers");
    }
    node = new_node_bop(t, &tyBool, lhs, rhs);
  }
}

static bool cast_numbers(Node **pLhs, Node **pRhs) {
  if (!is_number((*pLhs)->expType->type) || !is_number((*pRhs)->expType->type))
    return false;

  switch ((*pLhs)->expType->type) {
  case TY_INT:
    switch ((*pRhs)->expType->type) {
    case TY_INT:  return true;
    case TY_CHAR:
      *pRhs = new_node_cast(&tyInt, *pRhs, false);
      return true;
    default: break;
    }
    break;

  case TY_CHAR:
    switch ((*pRhs)->expType->type) {
    case TY_CHAR:  return true;
    case TY_INT:
      *pLhs = new_node_cast(&tyInt, *pLhs, false);
      return true;
    default: break;
    }
    break;

  default: break;
  }
  assert(false);
  return false;
}

static Node *eq(void) {
  Node *node = cmp();

  for (;;) {
    enum NodeType t;
    if (consume(TK_EQ))
      t = ND_EQ;
    else if (consume(TK_NE))
      t = ND_NE;
    else
      return node;

    Node *lhs = node, *rhs= add();
    if (lhs->expType->type == TY_PTR || rhs->expType->type == TY_PTR) {
      if (lhs->expType->type != TY_PTR || rhs->expType->type != TY_PTR ||
          !same_type(lhs->expType, rhs->expType))
        error("Cannot compare pointer to other types");
    } else {
      if (!cast_numbers(&lhs, &rhs))
        error("Cannot compare except numbers");
    }
    node = new_node_bop(t, &tyBool, lhs, rhs);
  }
}

static Node *logand(void) {
  Node *node = eq();
  for (;;) {
    if (consume(TK_LOGAND))
      node = new_node_bop(ND_LOGAND, &tyBool, node, eq());
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

static Node *assign(void) {
  Node *node = logior();

  if (consume(TK_ASSIGN))
    return new_node_bop(ND_ASSIGN, node->expType, node, new_node_cast(node->expType, assign(), false));
  if (consume(TK_ADD_ASSIGN))
    return new_node_unary(ND_ASSIGN_WITH, node->expType,
                          add_node(node, assign()));
  if (consume(TK_SUB_ASSIGN))
    return new_node_unary(ND_ASSIGN_WITH, node->expType,
                          sub_node(node, assign()));
  if (consume(TK_MUL_ASSIGN))
    return new_node_unary(ND_ASSIGN_WITH, node->expType,
                          arith_node(ND_MUL, node, assign()));
  if (consume(TK_DIV_ASSIGN))
    return new_node_unary(ND_ASSIGN_WITH, node->expType,
                          arith_node(ND_DIV, node, assign()));
  if (consume(TK_MOD_ASSIGN))
    return new_node_unary(ND_ASSIGN_WITH, node->expType,
                          arith_node(ND_MOD, node, assign()));

  return node;
}

static Node *expr(void) {
  return assign();
}

static Node *parse_block(void) {
  assert(curfunc != NULL);
  Scope *scope = enter_scope(curfunc, NULL);
  Vector *nodes = NULL;
  for (;;) {
    if (consume(TK_RBRACE))
      break;
    if (nodes == NULL)
      nodes = new_vector();
    vec_push(nodes, stmt());
  }
  exit_scope();
  return new_node_block(scope, nodes);
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
      return new_node_if(cond, tblock, fblock);
    }
  }
  error("Parse `if' failed: %s", current_line());
  return NULL;
}

static Node *parse_while(void) {
  if (consume(TK_LPAR)) {
    Node *cond = expr();
    if (consume(TK_RPAR)) {
      int save_flag = curloopflag;
      curloopflag = LF_BREAK | LF_CONTINUE;
      Node *body = stmt();
      curloopflag = save_flag;

      return new_node_while(cond, body);
    }
  }
  error("Parse `while' failed: %s", current_line());
  return NULL;
}

static Node *parse_do_while(void) {
  int save_flag = curloopflag;
  curloopflag = LF_BREAK | LF_CONTINUE;
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
  error("Parse `while' failed: %s", current_line());
  return NULL;
}

static Node *parse_for(void) {
  if (consume(TK_LPAR)) {
    Node *pre = NULL, *cond = NULL, *post = NULL;
    if ((consume(TK_SEMICOL) || (pre = expr(), consume(TK_SEMICOL))) &&
        (consume(TK_SEMICOL) || (cond = expr(), consume(TK_SEMICOL))) &&
        (consume(TK_RPAR) || (post = expr(), consume(TK_RPAR)))) {
      int save_flag = curloopflag;
      curloopflag = LF_BREAK | LF_CONTINUE;
      Node *body = stmt();
      curloopflag= save_flag;
      return new_node_for(pre, cond, post, body);
    }
  }
  error("Syntax error `for': %s", current_line());
  return NULL;
}

static Node *parse_break_continue(enum NodeType type) {
  if (!consume(TK_SEMICOL))
    error("`;' expected, but %s", current_line());
  return new_node(type, &tyVoid);
}

static Node *parse_return(void) {
  assert(curfunc != NULL);

  Node *val = NULL;
  if (consume(TK_SEMICOL)) {
    if (curfunc->rettype->type != TY_VOID)
      error("`return' required a value");
  } else {
    val = expr();
    if (!consume(TK_SEMICOL))
      error("`;' expected, but %s", current_line());

    if (curfunc->rettype->type == TY_VOID)
      error("void function `return' a value");
    val = new_node_cast(curfunc->rettype, val, false);
  }
  return new_node_return(val);
}

static bool parse_vardecl(Node **pnode) {
  assert(curfunc != NULL);

  Vector *inits = NULL;
  const Type *rawType = NULL;

  do {
    const Type *type;
    const char *name;
    if (!parse_var_def(&rawType, &type, &name, false)) {
      if (rawType != NULL)
        error("type expected, but %s", current_line());
      return false;
    }

    if (consume(TK_LBRACKET)) {
      Token *tok;
      if ((tok = consume(TK_INTLIT))) {  // TODO: Constant expression.
        int count = tok->intval;
        if (count < 0)
          error("Array size must be greater than 0, but %d", count);
        type = arrayof(type, count);
        if (!consume(TK_RBRACKET))
          error("`]' expected, but %s", current_line());
      }
    }
    scope_add(curscope, name, type);

    if (consume(TK_ASSIGN)) {
      Node *val = expr();
      Node *var = new_node_varref(name, type, false);
      Node *node = new_node_bop(ND_ASSIGN, type, var, new_node_cast(type, val, false));

      if (inits == NULL)
        inits = new_vector();
      vec_push(inits, node);
    }
  } while (consume(TK_COMMA));

  if (!consume(TK_SEMICOL))
    error("Semicolon expected, but %s", current_line());

  if (inits != NULL && inits->len == 1)
    *pnode = inits->data[0];
  else
    *pnode = new_node_block(NULL, inits);
  return true;
}

static Node *stmt(void) {
  Node *vardecl;
  if (parse_vardecl(&vardecl))
    return vardecl;

  if (consume(TK_SEMICOL))
    return new_node_block(NULL, NULL);

  if (consume(TK_LBRACE))
    return parse_block();

  if (consume(TK_IF))
    return parse_if();

  if (consume(TK_WHILE))
    return parse_while();

  if (consume(TK_DO))
    return parse_do_while();

  if (consume(TK_FOR))
    return parse_for();

  if (consume(TK_BREAK)) {
    if ((curloopflag & LF_BREAK) == 0)
      error("`break' cannot be used outside of loop");
    return parse_break_continue(ND_BREAK);
  }
  if (consume(TK_CONTINUE)) {
    if ((curloopflag & LF_CONTINUE) == 0)
      error("`continue' cannot be used outside of loop");
    return parse_break_continue(ND_CONTINUE);
  }

  if (consume(TK_RETURN))
    return parse_return();

  // expression statement.
  Node *node = assign();
  if (!consume(TK_SEMICOL))
    error("Semicolon required: %s", current_line());
  return node;
}

static Vector *funparams(void) {
  Vector *params = NULL;
  if (consume(TK_RPAR)) {
    // Arbitrary funparams.
  } else {
    params = new_vector();
    for (;;) {
      const Type *rawType = NULL;
      const Type *type;
      const char *name;
      if (!parse_var_def(&rawType, &type, &name, params->len == 0))
        error("type expected, but %s", current_line());

      if (type->type == TY_VOID) {  // fun(void)
        if (!consume(TK_RPAR))
          error("`)' expected, but %s", current_line());
        break;
      }

      // If the type is array, handle it as a pointer.
      type = array_to_ptr(type);

      var_add(params, name, type);
      if (consume(TK_RPAR))
        break;
      if (consume(TK_COMMA))
        continue;
      error("Comma or `}' expected, but %s", current_line());
    }
  }
  return params;
}

static Node *parse_defun(const Type *type, const char *ident) {
  Vector *params = funparams();

  VarInfo *def = find_global(ident);
  if (def == NULL) {
    define_global(new_func_type(type, params), ident);
  } else {
    if (def->type->type != TY_FUNC)
      error("Definition conflict: `%s'", ident);
    // TODO: Check type.
    // TODO: Check duplicated definition.
  }

  if (consume(TK_SEMICOL))  // Prototype declaration.
    return NULL;

  if (consume(TK_LBRACE)) {  // Definition.
    Node *node = new_node_defun(type, ident, params);
    curfunc = node->defun;

    Vector *stmts = new_vector();
    while (!consume(TK_RBRACE)) {
      vec_push(stmts, stmt());
    }
    node->defun->stmts = stmts;
    exit_scope();
    return node;
  }
  error("Defun failed: %s", current_line());
  return NULL;
}

static Node *toplevel(void) {
  const Type *type = parse_raw_type();
  if (type != NULL) {
    type = parse_type_modifier(type, true);
    if (type->type == TY_STRUCT && consume(TK_SEMICOL))  // Just struct definition.
      return NULL;

    Token *tok;
    if ((tok = consume(TK_IDENT))) {
      const char *ident = tok->ident;

      if (consume(TK_SEMICOL)) {  // Global variable declaration.
        if (type->type == TY_VOID)
          error("`void' not allowed");
        define_global(type, ident);
        return NULL;
      }

      if (consume(TK_LPAR)) {  // Function.
        return parse_defun(type, ident);
      }
      error("`(' expected, but %s", current_line());
      return NULL;
    }
    error("ident expected, but %s", current_line());
    return NULL;
  }
  error("Toplevel, %s", current_line());
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
