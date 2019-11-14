#pragma once

#include <stdbool.h>
#include <stdint.h>  // intptr_t

typedef struct Function Function;
typedef struct Expr Expr;
typedef struct Map Map;
typedef struct Scope Scope;
typedef struct Token Token;
typedef struct Type Type;
typedef struct Vector Vector;

// Defun

typedef struct Defun {
  Function *func;

  Vector *stmts;  // NULL => Prototype definition.

  Map *label_map;  // <const char*, BB*>
  Vector *gotos;

  int flag;
} Defun;

// Initializer

typedef struct Initializer {
  enum { vSingle, vMulti, vDot, vArr } kind;  // vSingle: 123, vMulti: {...}, vDot: .x=123, vArr: [n]=123
  union {
    Expr *single;
    Vector *multi;  // <Initializer*>
    struct {
      const char *name;
      struct Initializer *value;
    } dot;
    struct {
      Expr *index;
      struct Initializer *value;
    } arr;
  };
} Initializer;

// Node

enum NodeKind {
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
  ND_ASM,
  ND_TOPLEVEL,
};

typedef struct VarDecl {
  const Type *type;
  const Token *ident;
  Initializer *init;
  int flag;
} VarDecl;

typedef struct Node {
  enum NodeKind kind;
  union {
    Expr *expr;
    Defun *defun;
    struct {
      Scope *scope;
      Vector *nodes;
    } block;
    struct {
      Expr *cond;
      struct Node *tblock;
      struct Node *fblock;
    } if_;
    struct {
      Expr *value;
      struct Node *body;
      Vector *case_values;  // <intptr_t>
      bool has_default;
    } switch_;
    struct {
      Expr *value;
    } case_;
    struct {
      Expr *cond;
      struct Node *body;
    } while_;
    struct {
      Expr *pre;
      Expr *cond;
      Expr *post;
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
      Expr *val;
    } return_;
    struct {
      Vector *decls;  // <VarDecl*>
      Vector *inits;  // <Node*>
    } vardecl;
    struct {
      Expr *str;
    } asm_;
    struct {
      Vector *nodes;
    } toplevel;
  };
} Node;

Node *new_node_expr(Expr *e);
Node *new_top_node(Vector *nodes);

Vector *parse_program(Vector *nodes);
