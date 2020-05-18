// Semantic analysis

#pragma once

#include <stdbool.h>

typedef struct Defun Defun;
typedef struct Expr Expr;
typedef struct Initializer Initializer;
typedef struct Name Name;
typedef struct Scope Scope;
typedef struct Token Token;
typedef struct Type Type;
typedef struct VarInfo VarInfo;
typedef struct Vector Vector;

extern Defun *curdefun;
extern Scope *curscope;
extern Vector *toplevel;  // <Declaration*>

void sema(Vector *toplevel);
Expr *sema_expr(Expr *expr);
Expr *add_expr(const Token *tok, Expr *lhs, Expr *rhs);
Initializer *flatten_initializer(const Type *type, Initializer *init);
void ensure_struct(Type *type, const Token *token);
Expr *make_cast(const Type *type, const Token *token, Expr *sub, bool is_explicit);
const VarInfo *search_from_anonymous(const Type *type, const Name *name, const Token *ident,
                                     Vector *stack);
Expr *str_to_char_array(const Type *type, Initializer *init);
VarInfo *add_cur_scope(const Token *ident, const Type *type, int flag);
