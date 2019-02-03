#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "9cc.h"

char *strndup_(const char *str, size_t size) {
  char *dup = malloc(size + 1);
  strncpy(dup, str, size);
  dup[size] = '\0';
  return dup;
}

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

void tokenize(const char *p) {
  int i = 0;
  while (*p != '\0') {
    if (isspace(*p)) {
      ++p;
      continue;
    }

    if (*p == '=') {
      switch (p[1]) {
      case '=':
        alloc_token(TK_EQ, p);
        ++i;
        p += 2;
        continue;
      default:
        break;
      }
    }

    if (*p == '!' && p[1] == '=') {
      alloc_token(TK_NE, p);
      ++i;
      p += 2;
      continue;
    }

    if (strchr("+-*/(){}}=;,", *p) != NULL) {
      alloc_token((enum TokenType)*p, p);
      ++i;
      ++p;
      continue;
    }

    if (isdigit(*p)) {
      long val = strtol(p, (char**)&p, 10);
      Token *token = alloc_token(TK_NUM, p);
      token->val = val;
      ++i;
      continue;
    }

    if (isalpha(*p) || *p == '_') {
      const char *q;
      for (q = p + 1; ; ++q) {
        if (!(isalnum(*q) || *q == '_'))
          break;
      }

      Token *token = alloc_token(TK_IDENT, p);
      token->ident = strndup_(p, q - p);
      ++i;
      p = q;
      continue;
    }

    error("Unexpected character: %c\n", *p);
  }

  alloc_token(TK_EOF, p);
}

//

int var_find(Vector *lvars, const char *name) {
  int len = lvars->len;
  for (int i = 0; i < len; ++i) {
    if (strcmp(lvars->data[i], name) == 0)
      return i;
  }
  return -1;
}

int var_add(Vector *lvars, const char *name) {
  int idx = var_find(lvars, name);
  if (idx < 0) {
    idx = lvars->len;
    vec_push(lvars, (char*)name);
  }
  return idx;
}

//

static int pos;
static Node *curfunc;

Node *new_node_bop(enum NodeType type, Node *lhs, Node *rhs) {
  Node *node = malloc(sizeof(Node));
  node->type = type;
  node->bop.lhs = lhs;
  node->bop.rhs = rhs;
  return node;
}

Node *new_node_num(int val) {
  Node *node = malloc(sizeof(Node));
  node->type = ND_NUM;
  node->val = val;
  return node;
}

Node *new_node_ident(const char *name) {
  Node *node = malloc(sizeof(Node));
  node->type = ND_IDENT;
  node->ident = name;
  return node;
}

Node *new_node_defun(const char *name, Vector *params) {
  Node *node = malloc(sizeof(Node));
  node->type = ND_DEFUN;
  node->defun.name = name;
  node->defun.lvars = params;
  node->defun.param_count = params->len;
  node->defun.stmts = NULL;
  return node;
}

Node *new_node_funcall(const char *name, Vector *args) {
  Node *node = malloc(sizeof(Node));
  node->type = ND_FUNCALL;
  node->funcall.name = name;
  node->funcall.args = args;
  return node;
}

int consume(enum TokenType type) {
  if (get_token(pos)->type != type)
    return FALSE;
  ++pos;
  return TRUE;
}

Node *assign();

Node *funcall(const char *name) {
  Vector *args = NULL;
  if (!consume(TK_RPAR)) {
    args = new_vector();
    for (;;) {
      vec_push(args, assign());
      if (consume(TK_RPAR))
        break;
      if (consume(TK_COMMA))
        continue;
      error("Comma or `)` expected, but %s", get_token(pos)->input);
    }
  }
  return new_node_funcall(name, args);
}

Node *term() {
  if (consume(TK_LPAR)) {
    Node *node = assign();
    if (!consume(TK_RPAR))
      error("No close paren: %s", get_token(pos)->input);
    return node;
  }

  Token *token = get_token(pos);
  switch (token->type) {
  case TK_NUM:
    ++pos;
    return new_node_num(token->val);
  case TK_IDENT:
    ++pos;
    if (consume(TK_LPAR)) {
      return funcall(token->ident);
    } else {
      if (curfunc != NULL) {
        var_add(curfunc->defun.lvars, token->ident);
      }
      return new_node_ident(token->ident);
    }
  default:
    error("Number or Ident or open paren expected: %s", token->input);
    return NULL;
  }
}

Node *mul() {
  Node *node = term();

  for (;;) {
    if (consume(TK_MUL))
      node = new_node_bop(ND_MUL, node, term());
    else if (consume(TK_DIV))
      node = new_node_bop(ND_DIV, node, term());
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

Node *eq() {
  Node *node = add();

  for (;;) {
    if (consume(TK_EQ))
      node = new_node_bop(ND_EQ, node, add());
    else if (consume(TK_NE))
      node = new_node_bop(ND_NE, node, add());
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

Node *stmt() {
  Node *node = assign();
  if (!consume(TK_SEMICOL))
    error("Semicolon required: %s", get_token(pos)->input);
  return node;
}

Vector *funparams() {
  Vector *params = new_vector();
  if (!consume(TK_RPAR)) {
    for (;;) {
      if (!consume(TK_IDENT)) {
        error("Ident expected, but %s", get_token(pos)->input);
      }
      vec_push(params, (char*)get_token(pos - 1)->ident);
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
  if (consume(TK_IDENT)) {
    Token *funcname = get_token(pos - 1);
    if (consume(TK_LPAR)) {
      Vector *params = funparams();
      if (consume(TK_LBRACE)) {
        Node *node = new_node_defun(funcname->ident, params);
        curfunc = node;

        Vector *stmts = new_vector();
        while (!consume(TK_RBRACE)) {
          Node *st = stmt();
          vec_push(stmts, st);
        }
        node->defun.stmts = stmts;
        return node;
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
  while (get_token(pos)->type != TK_EOF)
    vec_push(node_vector, toplevel());
}
