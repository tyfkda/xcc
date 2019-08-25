#pragma once

#include <stdbool.h>

typedef struct Defun Defun;
typedef struct Expr Expr;
typedef struct Initializer Initializer;
typedef struct Node Node;
typedef struct Scope Scope;
typedef struct Token Token;
typedef struct Type Type;

extern Defun *curfunc;
extern Scope *curscope;

Node *sema(Node *node);

bool can_cast(const Type *dst, const Type *src, Expr *src_expr, bool is_explicit);
Initializer *flatten_initializer(const Type *type, Initializer *init);
void ensure_struct(Type *type, const Token *token);
Expr *make_cast(const Type *type, const Token *token, Expr *sub, bool is_explicit);
