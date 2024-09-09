#pragma once

typedef struct Expr Expr;
typedef struct Initializer Initializer;
typedef struct Scope Scope;
typedef struct Token Token;
typedef struct Type Type;
typedef struct Vector Vector;

Type *fix_array_size(Type *type, Initializer *init);
Expr *str_to_char_array_var(Scope *scope, Expr *str);
void construct_initializing_stmts(Vector *decls);
Vector *assign_initial_value(Expr *expr, Initializer *init, Vector *inits);
Initializer *flatten_initializer(Type *type, Initializer *init);
Initializer *check_vardecl(Type **ptype, const Token *ident, int storage, Initializer *init);
