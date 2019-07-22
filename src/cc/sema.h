#pragma once

typedef struct Defun Defun;
typedef struct Initializer Initializer;
typedef struct Node Node;
typedef struct Token Token;
typedef struct Type Type;

extern Defun *curfunc;

Node *sema(Node *node);

Initializer *flatten_initializer(const Type *type, Initializer *init);
void ensure_struct(Type *type, const Token *token);
