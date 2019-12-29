// Semantic analysis

#pragma once

#include <stdbool.h>

typedef struct Declaration Declaration;
typedef struct Defun Defun;
typedef struct Expr Expr;
typedef struct Initializer Initializer;
typedef struct Stmt Stmt;
typedef struct Scope Scope;
typedef struct Token Token;
typedef struct Type Type;
typedef struct VarInfo VarInfo;
typedef struct Vector Vector;

extern Defun *curdefun;
extern Scope *curscope;

Declaration *sema(Declaration *decl);
Expr *analyze_expr(Expr *expr, bool keep_left);
bool can_cast(const Type *dst, const Type *src, Expr *src_expr, bool is_explicit);
Initializer *flatten_initializer(const Type *type, Initializer *init);
void ensure_struct(Type *type, const Token *token);
Expr *make_cast(const Type *type, const Token *token, Expr *sub, bool is_explicit);
bool search_from_anonymous(const Type *type, const char *name, const Token *ident, Vector *stack);
VarInfo *str_to_char_array(const Type *type, Initializer *init);
