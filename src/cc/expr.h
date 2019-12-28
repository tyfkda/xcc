// Expression

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t

typedef struct Expr Expr;
typedef union Num Num;
typedef struct Scope Scope;
typedef struct Token Token;
typedef struct Type Type;
typedef struct Vector Vector;

//

const Type *parse_raw_type(int *pflag);
const Type *parse_type_modifier(const Type* type);
const Type *parse_type_suffix(const Type *type);
const Type *parse_full_type(int *pflag, Token **pident);

Expr *add_expr(const Token *tok, Expr *lhs, Expr *rhs, bool keep_left);
Vector *parse_args(Token **ptoken);
Vector *parse_funparams(bool *pvaargs);
Vector *parse_funparam_types(bool *pvaargs);  // Vector<Type*>
bool parse_var_def(const Type **prawType, const Type** ptype, int *pflag, Token **pident);
Expr *parse_const(void);
Expr *parse_assign(void);
Expr *parse_expr(void);
bool is_const(Expr *expr);
void not_void(const Type *type);
