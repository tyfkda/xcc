#pragma once

#include <stdbool.h>
#include <stdint.h>  // intptr_t

typedef struct Defun Defun;
typedef struct Expr Expr;
typedef struct Initializer Initializer;
typedef struct Map Map;
typedef struct Scope Scope;
typedef struct Token Token;
typedef struct Type Type;
typedef struct VarInfo VarInfo;
typedef struct Vector Vector;

// Node

enum NodeType {
  ND_EXPR,
  ND_DEFUN,
  ND_BLOCK,
  ND_IF,
  ND_SWITCH,
  ND_WHILE,
  ND_DO_WHILE,
  ND_FOR,
  ND_BREAK,
  ND_CONTINUE,
  ND_RETURN,
  ND_CASE,
  ND_DEFAULT,
  ND_GOTO,
  ND_LABEL,
  ND_VARDECL,
  ND_TOPLEVEL,
};

typedef struct VarDecl {
  const Type *type;
  const Token *ident;
  Initializer *init;
  int flag;
} VarDecl;

typedef struct Node {
  enum NodeType type;
  union {
    Expr *expr;
    Defun *defun;
    struct {
      Scope *scope;
      Vector *nodes;
    } block;
    struct {
      struct Expr *cond;
      struct Node *tblock;
      struct Node *fblock;
    } if_;
    struct {
      struct Expr *value;
      struct Node *body;
      Vector *case_values;
      bool has_default;
    } switch_;
    struct {
      intptr_t value;
    } case_;
    struct {
      struct Expr *cond;
      struct Node *body;
    } while_;
    struct {
      struct Expr *pre;
      struct Expr *cond;
      struct Expr *post;
      struct Node *body;
    } for_;
    struct {
      const Token *tok;
      const char *ident;
    } goto_;
    struct {
      const char *name;
      struct Node *stmt;
    } label;
    struct {
      struct Expr *val;
    } return_;
    struct {
      Vector *decls;  // <VarDecl*>
      Vector *inits;  // <Node*>
    } vardecl;
    struct {
      Vector *nodes;
    } toplevel;
  } u;
} Node;

Node *new_node_expr(Expr *e);

Node *parse_program(void);
