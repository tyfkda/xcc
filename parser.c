#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "xcc.h"

char *strndup_(const char *str, size_t size) {
  char *dup = malloc(size + 1);
  strncpy(dup, str, size);
  dup[size] = '\0';
  return dup;
}

const static Type tyVoid = {.type=TY_VOID, .ptrof=NULL};
const static Type tyInt = {.type=TY_INT, .ptrof=NULL};
const static Type tyChar = {.type=TY_CHAR, .ptrof=NULL};
const static Type tyStr = {.type=TY_PTR, .ptrof=&tyChar};

Vector *token_vector;

Token *alloc_token(enum TokenType type, const char *input) {
  Token *token = malloc(sizeof(*token));
  token->type = type;
  token->input = input;
  vec_push(token_vector, token);
  return token;
}

Token *get_token(int pos) {
  return (Token *)token_vector->data[pos];
}

enum TokenType reserved_word(const char *word) {
  struct {
    const char *str;
    enum TokenType type;
  } table[] = {
    { "if", TK_IF },
    { "else", TK_ELSE },
    { "do", TK_DO },
    { "while", TK_WHILE },
    { "for", TK_FOR },
    { "return", TK_RETURN },
    { "void", TK_KWVOID },
    { "int", TK_KWINT },
    { "char", TK_KWCHAR },
    { "struct", TK_STRUCT },
  };
  for (int i = 0; i < sizeof(table) / sizeof(*table); ++i) {
    if (strcmp(table[i].str, word) == 0)
      return table[i].type;
  }
  return -1;
}

char backslash(char c) {
  switch (c) {
  case '0':  return '\0';
  case 'n':  return '\n';
  case 't':  return '\t';
  case 'r':  return '\r';
  default:   return c;
  }
}

void tokenize(const char *p) {
  while (*p != '\0') {
    if (isspace(*p)) {
      ++p;
      continue;
    }

    if (*p == '=') {
      switch (p[1]) {
      case '=':
        alloc_token(TK_EQ, p);
        p += 2;
        continue;
      default:
        break;
      }
    }

    if (*p == '!' && p[1] == '=') {
      alloc_token(TK_NE, p);
      p += 2;
      continue;
    }

    if (*p == '<') {
      const char *input = p++;
      enum TokenType tk = TK_LT;
      if (*p == '=') {
        tk = TK_LE;
        ++p;
      }
      alloc_token(tk, input);
      continue;
    }

    if (*p == '>') {
      const char *input = p++;
      enum TokenType tk = TK_GT;
      if (*p == '=') {
        tk = TK_GE;
        ++p;
      }
      alloc_token(tk, input);
      continue;
    }

    if (*p == '-' && p[1] == '>') {
      alloc_token(TK_ARROW, p);
      p += 2;
      continue;
    }

    if (strchr("+-*/%&(){}[]=;,.", *p) != NULL) {
      alloc_token((enum TokenType)*p, p);
      ++p;
      continue;
    }

    if (isdigit(*p)) {
      long val = strtol(p, (char**)&p, 10);
      Token *token = alloc_token(TK_NUM, p);
      token->val = val;
      continue;
    }

    if (isalpha(*p) || *p == '_') {
      const char *q;
      for (q = p + 1; ; ++q) {
        if (!(isalnum(*q) || *q == '_'))
          break;
      }

      char *dup = strndup_(p, q - p);
      enum TokenType word = reserved_word(dup);
      if (word != -1) {
        alloc_token(word, p);
      } else {
        Token *token = alloc_token(TK_IDENT, p);
        token->ident = dup;
      }
      p = q;
      continue;
    }

    if (*p == '\'') {
      const char *start = p++;
      char c = *p;
      if (c == '\\') {
        c = *(++p);
        if (c == '\0')
          error("Character not closed");
        c = backslash(c);
      }
      if (*(++p) != '\'')
        error("Character not closed");

      Token *token = alloc_token(TK_CHAR, start);
      token->val = c;
      ++p;
      continue;
    }

    if (*p == '"') {
      const char *start = p++;
      size_t capa = 8, size = 0;
      char *str = malloc(capa);
      for (char c; (c = *p) != '"'; ++p) {
        if (c == '\0')
          error("String not closed");
        if (size + 1 >= capa) {
          capa <<= 1;
          str = realloc(str, capa);
        }

        if (c == '\\') {
          c = *(++p);
          if (c == '\0')
            error("String not closed");
          c = backslash(c);
        }
        str[size++] = c;
      }
      str[size] = '\0';
      Token *token = alloc_token(TK_STR, start);
      token->str = str;
      ++p;
      continue;
    }

    error("Unexpected character: %c\n", *p);
  }

  alloc_token(TK_EOF, p);
}

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

//

static int pos;
static Node *curfunc;

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
  // Clone params.
  Vector *newparams = new_vector();
  for (int i = 0; i < params->len; ++i)
    vec_push(newparams, params->data[i]);
  f->func.params = newparams;
  return f;
}

Node *new_node(enum NodeType type, const Type *expType) {
  Node *node = malloc(sizeof(Node));
  node->type = type;
  node->expType = expType;
  return node;
}

Node *new_node_bop(enum NodeType type, Node *lhs, Node *rhs) {
  const Type *expType = NULL;
  switch (type) {
  case ND_ASSIGN:
    // TODO: Check lhs and rhs types are equal.
    expType = lhs->expType;
    break;
  case ND_ADD:
    if (lhs->expType->type == TY_PTR) {
      if (rhs->expType->type == TY_PTR)
        error("Cannot add pointers");
      expType = lhs->expType;
    } else {
      expType = rhs->expType;  // Pointer or int.
    }
    break;
  case ND_SUB:
    if (lhs->expType->type == TY_PTR) {
      if (rhs->expType->type == TY_PTR)
        expType = &tyInt;
      else
        expType = lhs->expType;
    } else {
      if (lhs->expType->type == TY_PTR)
        error("Cannot sub pointer");
      expType = rhs->expType;  // int.
    }
    break;
  case ND_MUL:
  case ND_DIV:
    if (lhs->expType->type == TY_PTR || rhs->expType->type == TY_PTR) {
      error("Cannot sub pointers");
    } else {
      expType = lhs->expType;  // int.
    }
    break;
  default:
    //assert(FALSE);
    expType = lhs->expType;
    break;
  }

  assert(expType != NULL);

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
  Node *node = new_node(ND_DEFUN, &tyVoid);
  node->defun.rettype = rettype;
  node->defun.name = name;
  node->defun.lvars = params;
  node->defun.param_count = params->len;
  node->defun.stmts = NULL;
  node->defun.ret_label = NULL;
  return node;
}

Node *new_node_funcall(Node *func, Vector *args) {
  Node *node = new_node(ND_FUNCALL, func->expType->func.ret);
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

int consume(enum TokenType type) {
  if (get_token(pos)->type != type)
    return FALSE;
  ++pos;
  return TRUE;
}

Node *expr();

Node *funcall(Node *func) {
  if (func->expType->type != TY_FUNC)
    error("Cannot call except funtion");

  Vector *args = NULL;
  if (!consume(TK_RPAR)) {
    args = new_vector();
    for (;;) {
      vec_push(args, expr());
      if (consume(TK_RPAR))
        break;
      if (consume(TK_COMMA))
        continue;
      error("Comma or `)` expected, but %s", get_token(pos)->input);
    }
  }
  return new_node_funcall(func, args);
}

Node *array_index(Node *array) {
  Node *index = expr();
  if (!consume(TK_RBRACKET))
    error("`]' expected, but %s", get_token(pos)->input);
  return new_node_unary(ND_DEREF, new_node_bop(ND_ADD, array, index));
}

Node *member_access(Node *target) {
  if (!consume(TK_IDENT))
    error("`ident' expected, but %s", get_token(pos)->input);
  const char *name = get_token(pos - 1)->ident;

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
    Node *node = expr();
    if (!consume(TK_RPAR))
      error("No close paren: %s", get_token(pos)->input);
    return node;
  }

  Token *token = get_token(pos);
  switch (token->type) {
  case TK_NUM:
    ++pos;
    return new_node_num(token->val);
  case TK_CHAR:
    ++pos;
    return new_node_char(token->val);
  case TK_STR:
    ++pos;
    return new_node_str(token->str);
  case TK_IDENT:
    ++pos;
    if (curfunc == NULL) {
      error("Cannot use variable outside of function: `%s'", token->ident);
    } else {
      Type *type = NULL;
      int idx = var_find(curfunc->defun.lvars, token->ident);
      if (idx >= 0) {
        type = ((VarInfo*)curfunc->defun.lvars->data[idx])->type;
      } else {
        VarInfo *varinfo = find_global(token->ident);
        if (varinfo == NULL)
          error("Undefined `%s'", token->ident);
        type = varinfo->type;
      }
      if (type->type == TY_ARRAY)
        type = ptrof(type->ptrof);
      int global = idx < 0;
      return new_node_varref(token->ident, type, global);
    }
  default:
    error("Number or Ident or open paren expected: %s", token->input);
    return NULL;
  }
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
    else
      return node;
  }
}

Node *mul() {
  Node *node = term();

  for (;;) {
    if (consume(TK_MUL))
      node = new_node_bop(ND_MUL, node, term());
    else if (consume(TK_DIV))
      node = new_node_bop(ND_DIV, node, term());
    else if (consume(TK_MOD))
      node = new_node_bop(ND_MOD, node, term());
    else
      return node;
  }
}

Node *add() {
  Node *node = mul();

  for (;;) {
    if (consume(TK_ADD))
      node = new_node_bop(ND_ADD, node, mul());
    else if (consume(TK_SUB))
      node = new_node_bop(ND_SUB, node, mul());
    else
      return node;
  }
}

Node *cmp() {
  Node *node = add();

  for (;;) {
    if (consume(TK_LT))
      node = new_node_bop(ND_LT, node, add());
    else if (consume(TK_GT))
      node = new_node_bop(ND_GT, node, add());
    else if (consume(TK_LE))
      node = new_node_bop(ND_LE, node, add());
    else if (consume(TK_GE))
      node = new_node_bop(ND_GE, node, add());
    else
      return node;
  }
}

Node *eq() {
  Node *node = cmp();

  for (;;) {
    if (consume(TK_EQ))
      node = new_node_bop(ND_EQ, node, cmp());
    else if (consume(TK_NE))
      node = new_node_bop(ND_NE, node, cmp());
    else
      return node;
  }
}

Node *assign() {
  Node *node = eq();

  if (consume(TK_ASSIGN))
    return new_node_bop(ND_ASSIGN, node, assign());
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
  error("Parse `if' failed: %s", get_token(pos)->input);
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
  error("Parse `while' failed: %s", get_token(pos)->input);
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
  error("Parse `while' failed: %s", get_token(pos)->input);
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
  error("Syntax error `for': %s", get_token(pos)->input);
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
      error("`;' expected, but %s", get_token(pos)->input);

    // TODO: Check return type.
    if (curfunc->defun.rettype->type == TY_VOID)
      error("void function `return' a value");
  }
  return new_node_return(val);
}

Type *parse_type(void);

StructInfo *parse_struct() {
  Vector *members = new_vector();
  for (;;) {
    if (consume(TK_RBRACE))
      break;
    Type *type = parse_type();
    if (!consume(TK_IDENT))
      error("ident expected, but %s", get_token(pos)->input);
    const char *name = get_token(pos - 1)->ident;
    if (!consume(TK_SEMICOL))
      error("semicolon expected, but %s", get_token(pos)->input);
    var_add(members, name, type);
  }

  StructInfo *sinfo = malloc(sizeof(*sinfo));
  sinfo->members = members;
  sinfo->size = 0;
  sinfo->align = 0;
  return sinfo;
}

Type *parse_type(void) {
  Type *type = NULL;

  if (consume(TK_STRUCT)) {
    const char *name = NULL;
    if (consume(TK_IDENT))
      name = get_token(pos - 1)->ident;

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
  }

  return type;
}

void vardecl() {
  for (;;) {
    Type *type = parse_type();
    if (type == NULL)
      return;
    if (type->type == TY_VOID)
      error("Cannot use void for type");

    if (!consume(TK_IDENT))
      error("Ident expected, but %s", get_token(pos)->input);
    const char *name = get_token(pos - 1)->ident;

    if (consume(TK_LBRACKET)) {
      if (consume(TK_NUM)) {  // TODO: Constant expression.
        int count = get_token(pos - 1)->val;
        if (count < 0)
          error("Array size must be greater than 0, but %d", count);
        type = arrayof(type, count);
        if (!consume(TK_RBRACKET))
          error("`]' expected, but %s", get_token(pos)->input);
      }
    }
    if (!consume(TK_SEMICOL))
      error("Semicolon expected, but %s", get_token(pos)->input);
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
    error("Semicolon required: %s", get_token(pos)->input);
  return node;
}

Vector *funparams() {
  Vector *params = new_vector();
  if (!consume(TK_RPAR)) {
    for (;;) {
      Type *type = parse_type();
      if (type == NULL)
        error("type expected, but %s", get_token(pos)->input);

      if (!consume(TK_IDENT))
        error("Ident expected, but %s", get_token(pos)->input);
      var_add(params, get_token(pos - 1)->ident, type);
      if (consume(TK_RPAR))
        break;
      if (consume(TK_COMMA))
        continue;
      error("Comma or `}' expected, but %s", get_token(pos)->input);
    }
  }
  return params;
}

Node *toplevel() {
  Type *type = parse_type();
  if (type != NULL) {
    if (type->type == TY_STRUCT && consume(TK_SEMICOL))  // Just struct definition.
      return NULL;

    if (consume(TK_IDENT)) {
      const char *ident = get_token(pos - 1)->ident;

      if (consume(TK_SEMICOL)) {  // Global variable declaration.
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
    error("Defun failed: %s", get_token(pos)->input);
    return NULL;
  }
  error("Toplevel, %s", get_token(pos)->input);
  return NULL;
}

Vector *node_vector;

void program() {
  while (get_token(pos)->type != TK_EOF) {
    Node *node = toplevel();
    if (node != NULL)
      vec_push(node_vector, node);
  }
}
