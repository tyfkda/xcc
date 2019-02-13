#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "xcc.h"

const static Type tyVoid = {.type=TY_VOID, .ptrof=NULL};
const static Type tyInt = {.type=TY_INT, .ptrof=NULL};
const static Type tyChar = {.type=TY_CHAR, .ptrof=NULL};
const static Type tyStr = {.type=TY_PTR, .ptrof=&tyChar};
#define tyBool  tyInt

static Type *parse_type(bool allow_void);

//

int var_find(Vector *lvars, const char *name) {
  for (int i = 0, len = lvars->len; i < len; ++i) {
    VarInfo *info = (VarInfo*)lvars->data[i];
    if (strcmp(info->name, name) == 0)
      return i;
  }
  return -1;
}

void var_add(Vector *lvars, const char *name, Type *type) {
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

void define_global(Type *type, const char *name) {
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

static bool is_number(enum eType type) {
  return type == TY_INT || TY_CHAR;
}

static bool same_type(const Type *type1, const Type *type2) {
  for (;;) {
    if (type1->type != type2->type)
      return false;

    switch (type1->type) {
    case TY_VOID:
    case TY_INT:
    case TY_CHAR:
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

Type* ptrof(const Type *type) {
  Type *ptr = malloc(sizeof(*ptr));
  ptr->type = TY_PTR;
  ptr->ptrof = type;
  return ptr;
}

Type* arrayof(const Type *type, size_t array_size) {
  Type *arr = malloc(sizeof(*arr));
  arr->type = TY_ARRAY;
  arr->ptrof = type;
  arr->array_size = array_size;
  return arr;
}

Type* new_func_type(const Type *ret, const Vector *params) {
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

//

static Node *curfunc;

Node *new_node(enum NodeType type, const Type *expType) {
  Node *node = malloc(sizeof(Node));
  node->type = type;
  node->expType = expType;
  return node;
}

Node *new_node_cast(const Type *type, Node *sub, bool is_explicit) {
  if (same_type(type, sub->expType))
    return sub;

  if (type->type == TY_VOID || sub->expType->type == TY_VOID)
    error("cannot use `void' as a value");

  switch (type->type) {
  case TY_INT:
    switch (sub->expType->type) {
    case TY_CHAR:  goto ok;
    case TY_PTR:
      if (is_explicit) {
        // TODO: Check sizeof(int) is same as sizeof(ptr)
        goto ok;
      }
      break;
    default:  break;
    }
    break;
  case TY_CHAR:
    switch (sub->expType->type) {
    case TY_INT:  goto ok;  // TODO: Raise warning if implicit.
    default:  break;
    }
    break;
  case TY_PTR:
    switch (sub->expType->type) {
    case TY_INT:
      if (is_explicit)
        goto ok;
      break;
    case TY_PTR:
      // void* is interchangable with any pointer type.
      if (type->ptrof->type == TY_VOID || sub->expType->ptrof->type == TY_VOID)
        goto ok;
      break;
    default:  break;
    }
    break;
  default:
    break;
  }
  error("Cannot convert value from type %d to %d", sub->expType->type, type->type);
  return NULL;

 ok:;
  Node *node = new_node(ND_CAST, type);
  node->cast.sub = sub;
  return node;
}

Node *new_node_bop(enum NodeType type, const Type *expType, Node *lhs, Node *rhs) {
  Node *node = new_node(type, expType);
  node->bop.lhs = lhs;
  node->bop.rhs = rhs;
  return node;
}

Node *new_node_unary(enum NodeType type, Node *sub) {
  const Type *expType = NULL;
  switch (type) {
  case ND_REF:
    expType = ptrof(sub->expType);
    break;
  case ND_DEREF:
    if (sub->expType->type != TY_PTR)
      error("Cannot dereference raw type");
    expType = sub->expType->ptrof;
    break;
  default:
    expType = sub->expType;
    break;
  }

  assert(expType != NULL);

  Node *node = new_node(type, expType);
  node->unary.sub = sub;
  return node;
}

Node *new_node_num(long val) {
  Node *node = new_node(ND_NUM, &tyInt);
  node->val = val;
  return node;
}

Node *new_node_char(int val) {
  Node *node = new_node(ND_CHAR, &tyChar);
  node->val = val;
  return node;
}

Node *new_node_str(const char *str) {
  Node *node = new_node(ND_STR, &tyStr);
  node->str = str;
  return node;
}

Node *new_node_varref(const char *name, const Type *type, int global) {
  Node *node = new_node(ND_VARREF, type);
  node->varref.ident = name;
  node->varref.global = global;
  return node;
}

Node *new_node_member(Node *target, const char *name, const Type *expType) {
  Node *node = new_node(ND_MEMBER, expType);
  node->member.target = target;
  node->member.name = name;
  return node;
}

Node *new_node_defun(const Type *rettype, const char *name, Vector *params) {
  Vector *lvars = params != NULL ? params : new_vector();

  Node *node = new_node(ND_DEFUN, &tyVoid);
  node->defun.rettype = rettype;
  node->defun.name = name;
  node->defun.param_count = params != NULL ? params->len : -1;
  node->defun.lvars = lvars;
  node->defun.stmts = NULL;
  node->defun.ret_label = NULL;
  return node;
}

Node *new_node_funcall(Node *func, Vector *args) {
  const Type *rettype = func->expType->type == TY_FUNC ? func->expType->func.ret : &tyInt;  // TODO: Fix.
  Node *node = new_node(ND_FUNCALL, rettype);
  node->funcall.func = func;
  node->funcall.args = args;
  return node;
}

Node *new_node_block(Vector *nodes) {
  Node *node = new_node(ND_BLOCK, &tyVoid);
  node->block.nodes = nodes;
  return node;
}

Node *new_node_if(Node *cond, Node *tblock, Node *fblock) {
  Node *node = new_node(ND_IF, &tyVoid);
  node->if_.cond = cond;
  node->if_.tblock = tblock;
  node->if_.fblock = fblock;
  return node;
}

Node *new_node_while(Node *cond, Node *body) {
  Node *node = new_node(ND_WHILE, &tyVoid);
  node->while_.cond = cond;
  node->while_.body = body;
  return node;
}

Node *new_node_do_while(Node *body, Node *cond) {
  Node *node = new_node(ND_DO_WHILE, &tyVoid);
  node->do_while.body = body;
  node->do_while.cond = cond;
  return node;
}

Node *new_node_for(Node *pre, Node *cond, Node *post, Node *body) {
  Node *node = new_node(ND_FOR, &tyVoid);
  node->for_.pre = pre;
  node->for_.cond = cond;
  node->for_.post = post;
  node->for_.body = body;
  return node;
}

Node *new_node_return(Node *val) {
  const Type *type = val != NULL ? val->expType : &tyVoid;
  Node *node = new_node(ND_RETURN, type);
  node->return_.val = val;
  return node;
}

Node *expr();

Node *funcall(Node *func) {
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
  case TY_INT:
    switch (r->expType->type) {
    case TY_INT:
    case TY_CHAR:
      return new_node_bop(ND_ADD, l->expType, l, new_node_cast(l->expType, r, false));
    case TY_PTR:
      return new_node_bop(ND_PTRADD, r->expType, r, l);
    default:
      break;
    }
    break;

  case TY_CHAR:
    switch (r->expType->type) {
    case TY_CHAR:
      return new_node_bop(ND_ADD, l->expType, l, r);
    default:
      break;
    }
    break;

  default:
    break;
  }

  error("Illegal `+'");
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
    default:
      break;
    }
    break;

  default:
    break;
  }

  error("Illegal `-'");
  return NULL;
}

Node *array_index(Node *array) {
  Node *index = expr();
  if (!consume(TK_RBRACKET))
    error("`]' expected, but %s", current_line());
  return new_node_unary(ND_DEREF, add_node(array, index));
}

Node *member_access(Node *target) {
  Token *tok;
  if (!(tok = consume(TK_IDENT)))
    error("`ident' expected, but %s", current_line());
  const char *name = tok->ident;

  // Find member's type from struct info.
  const Type *type = target->expType;
  if (type->type == TY_PTR)
    type = target->expType->ptrof;
  if (type->type != TY_STRUCT)
    error("`.' for non struct value");

  int index = var_find(type->struct_->members, name);
  if (index < 0)
    error("`%s' doesn't exist in the struct");
  VarInfo *varinfo = (VarInfo*)type->struct_->members->data[index];

  return new_node_member(target, name, varinfo->type);
}

Node *prim() {
  if (consume(TK_LPAR)) {
    Type *type = parse_type(false);
    if (type != NULL) {  // Cast
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
  if ((tok = consume(TK_NUM))) {
    return new_node_num(tok->val);
  } else if ((tok = consume(TK_CHAR))) {
    return new_node_char(tok->val);
  } else if ((tok = consume(TK_STR))) {
    return new_node_str(tok->str);
  } else if ((tok = consume(TK_IDENT))) {
    if (curfunc == NULL) {
      error("Cannot use variable outside of function: `%s'", tok->ident);
    } else {
      const Type *type = NULL;
      int idx = var_find(curfunc->defun.lvars, tok->ident);
      if (idx >= 0) {
        type = ((VarInfo*)curfunc->defun.lvars->data[idx])->type;
      } else {
        VarInfo *varinfo = find_global(tok->ident);
        if (varinfo == NULL)
          error("Undefined `%s'", tok->ident);
        type = varinfo->type;
      }
      if (type->type == TY_ARRAY)
        type = ptrof(type->ptrof);
      int global = idx < 0;
      return new_node_varref(tok->ident, type, global);
    }
  } else {
    error("Number or Ident or open paren expected: %s", current_line());
  }
  return NULL;
}

Node *term() {
  if (consume(TK_AMP)) {
    Node *node = term();
    return new_node_unary(ND_REF, node);
  }

  if (consume(TK_MUL)) {
    Node *node = term();
    return new_node_unary(ND_DEREF, node);
  }

  if (consume(TK_INC)) {
    Node *node = term();
    return new_node_unary(ND_PREINC, node);
  }

  if (consume(TK_DEC)) {
    Node *node = term();
    return new_node_unary(ND_PREDEC, node);
  }

  Node *node = prim();

  for (;;) {
    if (consume(TK_LPAR))
      node = funcall(node);
    else if (consume(TK_LBRACKET))
      node = array_index(node);
    else if (consume(TK_DOT))
      node = member_access(node);
    else if (consume(TK_ARROW))
      node = member_access(node);
    else if (consume(TK_INC))
      node = new_node_unary(ND_POSTINC, node);
    else if (consume(TK_DEC))
      node = new_node_unary(ND_POSTDEC, node);
    else
      return node;
  }
}

Node *mul() {
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

    Node *lhs = node, *rhs = term();
    if (!is_number(lhs->expType->type) || !is_number(rhs->expType->type))
      error("Cannot use `%c' except numbers.", tt);
    node = new_node_bop(t, node->expType, lhs, rhs);
  }
}

Node *add() {
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

Node *cmp() {
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

Node *eq() {
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

Node *assign() {
  Node *node = eq();

  if (consume(TK_ASSIGN))
    return new_node_bop(ND_ASSIGN, node->expType, node, new_node_cast(node->expType, assign(), false));
  else
    return node;
}

Node *expr() {
  return assign();
}

Node *stmt();

Node *block() {
  Vector *nodes = new_vector();
  for (;;) {
    if (consume(TK_RBRACE))
      return new_node_block(nodes);
    vec_push(nodes, stmt());
  }
}

Node *stmt_if() {
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

Node *stmt_while() {
  if (consume(TK_LPAR)) {
    Node *cond = expr();
    if (consume(TK_RPAR)) {
      Node *body = stmt();
      return new_node_while(cond, body);
    }
  }
  error("Parse `while' failed: %s", current_line());
  return NULL;
}

Node *stmt_do_while() {
  Node *body = stmt();
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

Node *stmt_for() {
  if (consume(TK_LPAR)) {
    Node *pre = NULL, *cond = NULL, *post = NULL;
    if ((consume(TK_SEMICOL) || (pre = expr(), consume(TK_SEMICOL))) &&
        (consume(TK_SEMICOL) || (cond = expr(), consume(TK_SEMICOL))) &&
        (consume(TK_RPAR) || (post = expr(), consume(TK_RPAR)))) {
      Node *body = stmt();
      return new_node_for(pre, cond, post, body);
    }
  }
  error("Syntax error `for': %s", current_line());
  return NULL;
}

Node *stmt_return() {
  assert(curfunc != NULL);

  Node *val = NULL;
  if (consume(TK_SEMICOL)) {
    if (curfunc->defun.rettype->type != TY_VOID)
      error("`return' required a value");
  } else {
    val = expr();
    if (!consume(TK_SEMICOL))
      error("`;' expected, but %s", current_line());

    if (curfunc->defun.rettype->type == TY_VOID)
      error("void function `return' a value");
    val = new_node_cast(curfunc->defun.rettype, val, false);
  }
  return new_node_return(val);
}

StructInfo *parse_struct() {
  Vector *members = new_vector();
  for (;;) {
    if (consume(TK_RBRACE))
      break;
    Type *type = parse_type(false);
    Token *tok;
    if (!(tok = consume(TK_IDENT)))
      error("ident expected, but %s", current_line());
    const char *name = tok->ident;
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

static Type *parse_type(bool allow_void) {
  Type *type = NULL;

  if (consume(TK_STRUCT)) {
    const char *name = NULL;
    Token *tok;
    if ((tok = consume(TK_IDENT)))
      name = tok->ident;

    StructInfo *sinfo;
    if (consume(TK_LBRACE)) {  // Definition
      sinfo = parse_struct(name);
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
      TK_KWVOID, TK_KWINT, TK_KWCHAR,
    };
    static const enum eType kTypes[] = {
      TY_VOID, TY_INT, TY_CHAR,
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
  if (type != NULL) {
    while (consume(TK_MUL))
      type = ptrof(type);

    if (!allow_void && type->type == TY_VOID)
      error("`void' not allowed");
  }

  return type;
}

void vardecl() {
  for (;;) {
    Type *type = parse_type(false);
    if (type == NULL)
      return;
    if (type->type == TY_VOID)
      error("Cannot use void for type");

    Token *tok;
    if (!(tok = consume(TK_IDENT)))
      error("Ident expected, but %s", current_line());
    const char *name = tok->ident;

    if (consume(TK_LBRACKET)) {
      if ((tok = consume(TK_NUM))) {  // TODO: Constant expression.
        int count = tok->val;
        if (count < 0)
          error("Array size must be greater than 0, but %d", count);
        type = arrayof(type, count);
        if (!consume(TK_RBRACKET))
          error("`]' expected, but %s", current_line());
      }
    }
    if (!consume(TK_SEMICOL))
      error("Semicolon expected, but %s", current_line());
    assert(curfunc != NULL);
    var_add(curfunc->defun.lvars, name, type);
  }
}

Node *stmt() {
  if (consume(TK_LBRACE))
    return block();

  if (consume(TK_IF))
    return stmt_if();

  if (consume(TK_WHILE))
    return stmt_while();

  if (consume(TK_DO))
    return stmt_do_while();

  if (consume(TK_FOR))
    return stmt_for();

  if (consume(TK_RETURN))
    return stmt_return();

  // expression statement.
  Node *node = assign();
  if (!consume(TK_SEMICOL))
    error("Semicolon required: %s", current_line());
  return node;
}

Vector *funparams() {
  Vector *params = NULL;
  if (consume(TK_RPAR)) {
    // Arbitrary funparams.
  } else {
    params = new_vector();
    for (;;) {
      Type *type = parse_type(params->len == 0);
      if (type == NULL)
        error("type expected, but %s", current_line());
      if (type->type == TY_VOID) {  // fun(void)
        if (!consume(TK_RPAR))
          error("`)' expected, but %s", current_line());
        break;
      }

      Token *tok;
      if (!(tok = consume(TK_IDENT)))
        error("Ident expected, but %s", current_line());
      var_add(params, tok->ident, type);
      if (consume(TK_RPAR))
        break;
      if (consume(TK_COMMA))
        continue;
      error("Comma or `}' expected, but %s", current_line());
    }
  }
  return params;
}

Node *toplevel() {
  Type *type = parse_type(true);
  if (type != NULL) {
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

      if (consume(TK_LPAR)) {  // Function definition.
        Vector *params = funparams();
        if (consume(TK_LBRACE)) {
          Node *node = new_node_defun(type, ident, params);
          define_global(new_func_type(type, params), ident);
          curfunc = node;

          vardecl();

          Vector *stmts = new_vector();
          while (!consume(TK_RBRACE)) {
            Node *st = stmt();
            vec_push(stmts, st);
          }
          node->defun.stmts = stmts;
          return node;
        }
      }
    }
    error("Defun failed: %s", current_line());
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
