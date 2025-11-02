#pragma once

#include <stddef.h>  // size_t

typedef struct Expr Expr;
typedef struct Name Name;
typedef struct Scope Scope;
typedef struct Token Token;
typedef struct Type Type;
typedef struct Vector Vector;

// Initializer

enum InitializerKind {
  IK_SINGLE,  // 123
  IK_MULTI,   // {...}
  IK_DOT,     // .x
  IK_BRKT,    // [n]
};

typedef struct Initializer {
  enum InitializerKind kind;
  const Token *token;
  union {
    Expr *single;
    Vector *multi;  // <Initializer*>
    struct {
      const Name *name;
      struct Initializer *value;
    } dot;
    struct {
      size_t index;
      struct Initializer *value;
    } bracket;
  };
} Initializer;

Initializer *new_initializer(enum InitializerKind kind, const Token *token);

Type *fix_array_size(Type *type, Initializer *init, const Token *token);
Expr *str_to_char_array_var(Scope *scope, Expr *str);
void construct_initializing_stmts(Vector *decls);
Vector *assign_initial_value(Expr *expr, Initializer *init, Vector *inits);
Initializer *flatten_initializer(Type *type, Initializer *init);
Initializer *check_vardecl(Type **ptype, const Token *ident, int storage, Initializer *init);
