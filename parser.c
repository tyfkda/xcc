#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "9cc.h"

Vector *token_vector;

Token *alloc_token(int ty, const char *input) {
  Token *token = malloc(sizeof(*token));
  token->ty = ty;
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

    if (*p == '+' || *p == '-' || *p == '*' || *p == '/' || *p == '(' || *p == ')' || *p == '=' || *p == ';') {
      /*Token *token =*/ alloc_token(*p, p);
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

      size_t len = q - p;
      char *dup = malloc(len + 1);
      memcpy(dup, p, len);
      dup[len] = '\0';

      Token *token = alloc_token(TK_IDENT, p);
      token->ident = dup;
      ++i;
      p = q;
      continue;
    }

    fprintf(stderr, "Cannot tokenize: %s\n", p);
    exit(1);
  }

  alloc_token(TK_EOF, p);
}

//

Vector *var_vector;

int var_find(const char *name) {
  int len = var_vector->len;
  for (int i = 0; i < len; ++i) {
    if (strcmp(var_vector->data[i], name) == 0)
      return i;
  }
  return -1;
}

int var_add(const char *name) {
  int idx = var_find(name);
  if (idx < 0) {
    idx = var_vector->len;
    vec_push(var_vector, (char*)name);
  }
  return idx;
}

//

int pos;

Node *new_node(int ty, Node *lhs, Node *rhs) {
  Node *node = malloc(sizeof(Node));
  node->ty = ty;
  node->bop.lhs = lhs;
  node->bop.rhs = rhs;
  return node;
}

Node *new_node_num(int val) {
  Node *node = malloc(sizeof(Node));
  node->ty = ND_NUM;
  node->val = val;
  return node;
}

Node *new_node_ident(const char *name) {
  Node *node = malloc(sizeof(Node));
  node->ty = ND_IDENT;
  node->varidx = var_add(name);
  return node;
}

int consume(int ty) {
  if (get_token(pos)->ty != ty)
    return FALSE;
  ++pos;
  return TRUE;
}

Node *assign();

Node *term() {
  if (consume('(')) {
    Node *node = assign();
    if (!consume(')'))
      error("No close paren: %s", get_token(pos)->input);
    return node;
  }

  Token *token = get_token(pos);
  switch (token->ty) {
  case TK_NUM:
    ++pos;
    return new_node_num(token->val);
  case TK_IDENT:
    ++pos;
    return new_node_ident(token->ident);
  }

  error("Number or Ident or open paren expected: %s", token->input);
  return NULL;
}

Node *mul() {
  Node *node = term();

  for (;;) {
    if (consume('*'))
      node = new_node('*', node, term());
    else if (consume('/'))
      node = new_node('/', node, term());
    else
      return node;
  }
}

Node *add() {
  Node *node = mul();

  for (;;) {
    if (consume('+'))
      node = new_node('+', node, mul());
    else if (consume('-'))
      node = new_node('-', node, mul());
    else
      return node;
  }
}

Node *assign() {
  Node *node = add();

  if (consume('='))
    return new_node('=', node, assign());
  else
    return node;
}

Node *stmt() {
  Node *node = assign();
  if (!consume(';'))
    error("Semicolon required: %s", get_token(pos)->input);
  return node;
}

Vector *node_vector;

void program() {
  while (get_token(pos)->ty != TK_EOF)
    vec_push(node_vector, stmt());
}
