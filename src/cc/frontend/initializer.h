#pragma once

#include "ast.h"  // Fixnum

typedef struct Expr Expr;
typedef struct Initializer Initializer;
typedef struct MemberInfo MemberInfo;
typedef struct Scope Scope;
typedef struct Stmt Stmt;
typedef struct StructInfo StructInfo;
typedef struct Type Type;
typedef struct VarInfo VarInfo;
typedef struct Vector Vector;

Type *fix_array_size(Type *type, Initializer *init);
Expr *str_to_char_array_var(Scope *scope, Expr *str);
void construct_initializing_stmts(Vector *decls);
Expr *assign_bitfield_member(const Token *tok, Expr *dst, Expr *src, Expr *val, const MemberInfo *minfo);
Expr *assign_to_bitfield(const Token *tok, Expr *lhs, Expr *rhs, const MemberInfo *minfo);
Vector *assign_initial_value(Expr *expr, Initializer *init, Vector *inits);
Initializer *flatten_initializer(Type *type, Initializer *init);
Fixnum calc_bitfield_initial_value(const StructInfo *sinfo, const Initializer *init, int *pi);
Initializer *check_vardecl(Type **ptype, const Token *ident, int storage, Initializer *init);
