#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "xcc.h"
#include "util.h"

static const Type tyVoid = {.type=TY_VOID};
static const Type tyInt = {.type=TY_INT};
static const Type tyChar = {.type=TY_CHAR};
static const Type tyLong = {.type=TY_LONG};
static const Type tyEnum = {.type=TY_ENUM};
#define tyBool  tyInt
#define tySize  tyLong

static StructInfo *parse_struct(void);
static Node *stmt(void);
static Node *expr(void);
static Node *cast_expr(void);
static Node *prim(void);

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

// Typedef

Map *typedef_map;

// Global

Map *global;

GlobalVarInfo *find_global(const char *name) {
  return (GlobalVarInfo*)map_get(global, name);
}

void define_global(const Type *type, const char *name, Node *value) {
  GlobalVarInfo *varinfo = find_global(name);
  if (varinfo != NULL)
    error("`%s' already defined", name);
  varinfo = malloc(sizeof(*varinfo));
  varinfo->name = name;
  varinfo->type = type;
  varinfo->value = value;
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
  case TY_PTR: dump_type(fp, type->u.pa.ptrof); fprintf(fp, "*"); break;
  case TY_ARRAY: dump_type(fp, type->u.pa.ptrof); fprintf(fp, "[%d]", (int)type->u.pa.length); break;
  default: assert(false);
  }
}

static bool is_number(enum eType type) {
  switch (type) {
  case TY_CHAR:
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
      return type1 == type2;
    case TY_STRUCT:
    case TY_UNION:
      return type1->u.struct_ == type2->u.struct_;
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
  defun->params = params;
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

static bool can_cast(const Type *dst, const Type *src, bool is_explicit) {
  if (same_type(dst, src))
    return true;

  if (dst->type == TY_VOID || src->type == TY_VOID)
    return false;

  switch (dst->type) {
  case TY_CHAR:
    switch (src->type) {
    case TY_INT:
    case TY_LONG:
      return true;  // TODO: Raise warning if implicit.
    default:  break;
    }
    break;
  case TY_INT:
    switch (src->type) {
    case TY_CHAR:
    case TY_LONG:
    case TY_ENUM:
      return true;
    default:  break;
    }
    break;
  case TY_LONG:
    switch (src->type) {
    case TY_CHAR:
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
    case TY_INT:  // TODO: Disable.
    case TY_LONG:
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
      if (same_type(dst->u.pa.ptrof, src->u.pa.ptrof))
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
    error("cannot use `void' as a value");

  if (!can_cast(type, sub->expType, is_explicit))
    error("Cannot convert value from type %d to %d: %s", sub->expType->type, type->type, current_line());

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
     error("Cannot dereference raw type");
  return new_node_unary(ND_DEREF, sub->expType->u.pa.ptrof, sub);
}

static Node *new_node_numlit(enum NodeType nodetype, intptr_t val) {
  const Type *type = NULL;
  switch (nodetype) {
  case ND_CHAR:  type = &tyChar; break;
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

static Node *new_node_varref(const char *name, const Type *type, int global) {
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

static Node *new_node_if(Node *cond, Node *tblock, Node *fblock) {
  Node *node = new_node(ND_IF, &tyVoid);
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
  if (!(func->expType->type == TY_FUNC ||
        func->expType->type == TY_PTR))  // TODO: Restrict to function pointer.
    error("Cannot call except funtion");

  const Type *functype = func->expType->type == TY_FUNC ? func->expType : NULL;

  Vector *args = NULL;
  if (!consume(TK_RPAR)) {
    args = new_vector();
    for (;;) {
      Node *arg = expr();
      if (functype != NULL && functype->u.func.params != NULL && args->len < functype->u.func.params->len) {
        const Type * type = ((VarInfo*)functype->u.func.params->data[args->len])->type;
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

  if (functype != NULL && functype->u.func.params != NULL && (args != NULL ? args->len : 0) != functype->u.func.params->len)
    error("function `%s' expect %d arguments, but %d\n", func->u.varref.ident, functype->u.func.params->len, (args != NULL ? args->len : 0));

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
      return new_node_bop(ND_ADD, r->expType, new_node_cast(r->expType, l, false), r);
    case TY_LONG:
      return new_node_bop(ND_ADD, r->expType, new_node_cast(r->expType, l, false), r);
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
      return new_node_bop(ND_PTRADD, r->expType, r, l);
    case TY_ARRAY:
      return new_node_bop(ND_PTRADD, array_to_ptr(r->expType), r, l);
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

  error("Illegal `+' at %s", current_line());
  return NULL;
}

static Node *sub_node(Node *lhs, Node *rhs) {
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

  case TY_INT:
    switch (rhs->expType->type) {
    case TY_INT:
    case TY_CHAR:
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
        error("Different pointer sub");
      return new_node_bop(ND_PTRDIFF, &tyInt, lhs, rhs);  // TODO: size_t
    case TY_ARRAY:
      if (!same_type(lhs->expType->u.pa.ptrof, rhs->expType->u.pa.ptrof))
        error("Different pointer sub");
      return new_node_bop(ND_PTRDIFF, &tyInt, lhs, rhs);  // TODO: size_t
    default:
      break;
    }
    break;

  case TY_ARRAY:
    switch (rhs->expType->type) {
    case TY_PTR:
      if (!same_type(lhs->expType->u.pa.ptrof, rhs->expType->u.pa.ptrof))
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
  const char *name = tok->u.ident;

  // Find member's type from struct info.
  const Type *type = target->expType;
  if (toktype == TK_DOT) {
    if (!is_struct_or_union(type->type))
      error("`.' for non struct value");
  } else {  // TK_ARROW
    if (type->type == TY_PTR)
      type = target->expType->u.pa.ptrof;
    else if (type->type == TY_ARRAY)
      type = target->expType->u.pa.ptrof;
    else
      error("`->' for non pointer value");
    if (type->type != TY_STRUCT)
      error("`->' for non struct value");
  }

  int index = var_find(type->u.struct_->members, name);
  if (index < 0)
    error("`%s' doesn't exist in the struct");
  VarInfo *varinfo = (VarInfo*)type->u.struct_->members->data[index];

  return new_node_member(target, name, varinfo->type);
}

static const Type *parse_enum(void) {
  const char *name = NULL;
  Token *tok;
  if ((tok = consume(TK_IDENT)))
    name = tok->u.ident;

  if (consume(TK_LBRACE)) {
    if (!consume(TK_RBRACE)) {
      int value = 0;
      for (;;) {
        Token *ident = consume(TK_IDENT);
        if (ident == NULL)
          error("ident expected, but %s", current_line());
        if (consume(TK_ASSIGN)) {
          Node *node = expr();
          if (node->type != ND_INT)  // TODO: Accept constexpr.
            error("const expected for const");
          value = node->u.value;
        }
        // Define
        (void)name;  // TODO: Define enum type with name.
        define_global(&tyEnum, ident->u.ident, new_node_numlit(ND_INT, value));
        ++value;

        if (consume(TK_RBRACE))
          break;
        if (!consume(TK_COMMA))
          error("`,' or `}' expected, but %s", current_line());
      }
    }
  }
  return &tyEnum;
}

static const Type *parse_raw_type(void) {
  Type *type = NULL;
  enum eType et;
  Token *ident;

  if ((consume(TK_STRUCT) && (et = TY_STRUCT, true)) ||
      (consume(TK_UNION) && (et = TY_UNION, true))) {
    const char *name = NULL;
    Token *tok;
    if ((tok = consume(TK_IDENT)))
      name = tok->u.ident;

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
    type->type = et;
    type->u.struct_ = sinfo;
  } else if (consume(TK_ENUM)) {
    return parse_enum();
  } else if ((ident = consume(TK_IDENT)) != NULL) {
    type = map_get(typedef_map, ident->u.ident);
    if (type == NULL)
      unget_token(ident);
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

static const Type *parse_type_suffix(const Type *type) {
  if (!consume(TK_LBRACKET))
    return type;
  Token *tok;
  size_t length;
  if (consume(TK_RBRACKET)) {
    length = -1;
  } else if ((tok = consume(TK_INTLIT))) {  // TODO: Constant expression.
    if (tok->u.value <= 0)
      error("Array size must be greater than 0, but %d", (int)tok->u.value);
    length = tok->u.value;
    if (!consume(TK_RBRACKET))
      error("`]' expected, but %s", current_line());
  } else {
    error("syntax error: %s", current_line());
  }
  return arrayof(parse_type_suffix(type), length);
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
    name = tok->u.ident;
    type = parse_type_suffix(type);
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

static Node *parse_sizeof(void) {
  const Type *type;
  if (consume(TK_LPAR)) {
    type = parse_raw_type();
    if (type != NULL) {  // Type
      type = parse_type_suffix(parse_type_modifier(type, true));
    } else {
      Node *node = expr();
      type = node->expType;
    }
    if (!consume(TK_RPAR))
      error("`)' expected, but %s", current_line());
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
      error("No close paren: %s", current_line());
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

  if ((tok = consume(TK_IDENT))) {
    assert(curfunc != NULL);

    const Type *type = NULL;
    VarInfo *varinfo = scope_find(curscope, tok->u.ident);
    if (varinfo != NULL) {
      type = varinfo->type;
    } else {
      GlobalVarInfo *varinfo = find_global(tok->u.ident);
      if (varinfo == NULL)
        error("Undefined `%s'", tok->u.ident);
      type = varinfo->type;
      if (type->type == TY_ENUM)
        // Enum value is embeded directly.
        return varinfo->value;
    }
    int global = varinfo == NULL;
    return new_node_varref(tok->u.ident, type, global);
  }
  error("Number or Ident or open paren expected: %s", current_line());
  return NULL;
}

static Node *postfix(void) {
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

static Node *unary(void) {
  if (consume(TK_ADD)) {
    Node *node = cast_expr();
    if (!is_number(node->expType->type))
      error("Cannot apply `+' except number types");
    return node;
  }

  if (consume(TK_SUB)) {
    Node *node = cast_expr();
    if (!is_number(node->expType->type))
      error("Cannot apply `-' except number types");
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

  if (consume(TK_NOT)) {
    Node *node = cast_expr();
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
    const Type *type = parse_raw_type();
    if (type != NULL) {  // Cast
      type = parse_type_suffix(parse_type_modifier(type, true));
      if (!consume(TK_RPAR))
        error("`)' expected, but %s", current_line());
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
    if (consume(TK_MUL))
      t = ND_MUL;
    else if (consume(TK_DIV))
      t = ND_DIV;
    else if (consume(TK_MOD))
      t = ND_MOD;
    else
      return node;

    node = arith_node(t, node, cast_expr());
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
  enum eType ltype = (*pLhs)->expType->type, rtype = (*pRhs)->expType->type;
  if (!is_number(ltype) || !is_number(rtype))
    return false;

  if (ltype <= rtype)
    *pLhs = new_node_cast((*pRhs)->expType, *pLhs, false);
  else
    *pRhs = new_node_cast((*pLhs)->expType, *pRhs, false);
  return true;
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
  error("Parse `switch' failed: %s", current_line());
  return NULL;
}

static Node *parse_case(void) {
  if (curswitch == NULL)
    error("`case' cannot use outside of `switch`: %s", current_line());

  Node *valnode = expr();
  intptr_t value;
  switch (valnode->type) {  // TODO: Accept const expression.
  case ND_CHAR:
  case ND_INT:
  case ND_LONG:
    value = valnode->u.value;
    break;
  default:
    error("Cannot use expression, but %s", current_line());
    break;
  }
  if (!consume(TK_COLON))
    error("`:' expected, but %s", current_line());

  Vector *values = curswitch->u.switch_.case_values;
  if (values == NULL)
    curswitch->u.switch_.case_values = values = new_vector();

  // Check duplication.
  for (int i = 0, len = values->len; i < len; ++i) {
    if ((intptr_t)values->data[i] == value)
      error("Case value `%lld' already defined: %s", value, current_line());
  }

  vec_push(values, (void*)value);

  return new_node_case(value);
}

static Node *parse_default(void) {
  if (curswitch == NULL)
    error("`default' cannot use outside of `switch`: %s", current_line());
  if (curswitch->u.switch_.has_default)
    error("`default' already defined in `switch`: %s", current_line());

  if (!consume(TK_COLON))
    error("`:' expected, but %s", current_line());

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
  error("Parse `while' failed: %s", current_line());
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
      curloopflag |= LF_BREAK | LF_CONTINUE;
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

typedef struct {
  enum { vSingle, vMulti } type;
  union {
    Node *single;
    Vector *multi;  // Initializer*
  } u;
} Initializer;

static Initializer *parse_initializer(void) {
  Initializer *result = malloc(sizeof(*result));
  if (consume(TK_LBRACE)) {
    Vector *multi = new_vector();
    for (;;) {
      Initializer *elem = parse_initializer();
      vec_push(multi, elem);

      if (consume(TK_COMMA)) {
        if (consume(TK_RBRACE))
          break;
      } else {
        if (!consume(TK_RBRACE))
          error("`}' or `,' expected, but %s", current_line());
        break;
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
  case TY_ARRAY:
    {
      size_t arr_len = node->expType->u.pa.length;
      for (size_t i = 0; i < arr_len; ++i)
        clear_initial_value(new_node_deref(add_node(node, new_node_numlit(ND_INT, i))), inits);
    }
    break;
  case TY_STRUCT:
  case TY_UNION:
    assert(!"Not implemented");
    break;
  default:
    vec_push(inits,
             new_node_bop(ND_ASSIGN, node->expType, node, new_node_cast(node->expType, new_node_numlit(ND_INT, 0), false)));
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
      error("Buffer is shorter than string: %d for \"%s\"", (int)dstlen, str);
  }

  for (size_t i = 0; i < len; ++i) {
    Node *index = new_node_numlit(ND_INT, i);
    vec_push(inits,
             new_node_bop(ND_ASSIGN, &tyChar,
                          new_node_deref(add_node(dst, index)),
                          new_node_deref(add_node(src, index))));
  }
  if (dstlen > len) {
    Node *zero = new_node_numlit(ND_CHAR, 0);
    for (size_t i = len; i < dstlen; ++i) {
      Node *index = new_node_numlit(ND_INT, i);
      vec_push(inits,
               new_node_bop(ND_ASSIGN, &tyChar,
                            new_node_deref(add_node(dst, index)),
                            zero));
    }
  }
}

static Vector *assign_initial_value(Node *node, Initializer *initializer, Vector *inits) {
  if (inits == NULL)
    inits = new_vector();

  switch (node->expType->type) {
  case TY_ARRAY:
    {
      // Special handling for string (char[]).
      if (node->expType->u.pa.ptrof->type == TY_CHAR &&
          initializer->type == vSingle &&
          can_cast(node->expType, initializer->u.single->expType, false)) {
        string_initializer(node, initializer->u.single, inits);
        break;
      }

      if (initializer->type != vMulti)
        error("Error initializer");
      size_t arr_len = node->expType->u.pa.length;
      if (arr_len == (size_t)-1) {
        ((Type*)node->expType)->u.pa.length = arr_len = initializer->u.multi->len;
      } else {
        if ((size_t)initializer->u.multi->len > arr_len)
          error("Initializer more than array size");
      }
      int len = initializer->u.multi->len;
      for (int i = 0; i < len; ++i) {
        assign_initial_value(new_node_deref(add_node(node, new_node_numlit(ND_INT, i))),
                             initializer->u.multi->data[i], inits);
      }
      // Clear left.
      for (size_t i = len; i < arr_len; ++i)
        clear_initial_value(new_node_deref(add_node(node, new_node_numlit(ND_INT, i))), inits);
    }
    break;
  case TY_STRUCT:
  case TY_UNION:
    assert(!"Not implemented");
    break;
  default:
    if (initializer->type != vSingle)
      error("Error initializer");
    vec_push(inits,
             new_node_bop(ND_ASSIGN, node->expType, node, new_node_cast(node->expType, initializer->u.single, false)));
    break;
  }

  return inits;
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

    scope_add(curscope, name, type);

    if (consume(TK_ASSIGN)) {
      Initializer *initializer = parse_initializer();
      inits = assign_initial_value(new_node_varref(name, type, false), initializer, inits);
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

// Multiple stmt-s, also accept `case` and `default`.
static Vector *read_stmts(void) {
  Vector *nodes = NULL;
  for (;;) {
    if (consume(TK_RBRACE))
      return nodes;
    if (nodes == NULL)
      nodes = new_vector();

    Node *node;
    if (parse_vardecl(&node))
      ;
    else if (consume(TK_CASE))
      node = parse_case();
    else if (consume(TK_DEFAULT))
      node = parse_default();
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

  Defun *defun = NULL;
  if (consume(TK_SEMICOL)) {  // Prototype declaration.
  } else {
    if (!consume(TK_LBRACE)) {
      error("Defun failed: %s", current_line());
      return NULL;
    }
    // Definition.
    defun = new_defun(type, ident, params);
  }

  GlobalVarInfo *def = find_global(ident);
  if (def == NULL) {
    define_global(new_func_type(type, params), ident, NULL);
  } else {
    if (def->type->type != TY_FUNC)
      error("Definition conflict: `%s'", ident);
    // TODO: Check type.
    // TODO: Check duplicated definition.
    if (def->value != NULL)
      error("`%s' function already defined", ident);
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

static void parse_global_assign(const Type *type, const char *name) {
  Node *value = expr();
  if (!consume(TK_SEMICOL))
    error("`;' expected, but %s", current_line());
  /*Node *newvalue =*/ new_node_cast(type, value, false);
  define_global(type, name, value);  // TODO: Use newvalue
}

static void parse_typedef(void) {
  const Type *type = parse_raw_type();
  if (type == NULL)
    error("type expected, but %s", current_line());
  type = parse_type_suffix(parse_type_modifier(type, false));

  Token *ident = consume(TK_IDENT);
  if (ident == NULL)
    error("ident expected, but %s", current_line());
  const char *name = ident->u.ident;

  map_put(typedef_map, name, type);

  if (!consume(TK_SEMICOL))
    error("`;' expected, but %s", current_line());
}

static Node *toplevel(void) {
  const Type *type = parse_raw_type();
  if (type != NULL) {
    type = parse_type_modifier(type, true);
    if ((is_struct_or_union(type->type) || type->type == TY_ENUM) &&
        consume(TK_SEMICOL))  // Just struct/union definition.
      return NULL;

    Token *tok;
    if ((tok = consume(TK_IDENT))) {
      const char *ident = tok->u.ident;

      if (consume(TK_LPAR))  // Function.
        return parse_defun(type, ident);

      if (type->type == TY_VOID)
        error("`void' not allowed");

      type = parse_type_suffix(type);
      if (consume(TK_SEMICOL)) {  // Global variable declaration.
        define_global(type, ident, NULL);
        return NULL;
      }
      if (consume(TK_ASSIGN)) {
        parse_global_assign(type, ident);
        return NULL;
      }

      error("`;' or `=' expected, but %s", current_line());
      return NULL;
    }
    error("ident expected, but %s", current_line());
    return NULL;
  }
  if (consume(TK_TYPEDEF)) {
    parse_typedef();
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
