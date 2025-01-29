// Parser

#pragma once

#include <stdbool.h>

#include "ast.h"  // TokenKind

typedef struct Expr Expr;
typedef struct Initializer Initializer;
typedef struct Stmt Stmt;
typedef struct Token Token;
typedef struct Type Type;
typedef struct Vector Vector;

void parse(Vector *decls);  // <Declaration*>

//

typedef Expr *(*BuiltinExprProc)(const Token*);
void add_builtin_expr_ident(const char *str, BuiltinExprProc *proc);

Type *parse_raw_type(int *pstorage);
Type *parse_type_suffix(Type *type);
Type *parse_declarator(Type *rawtype, Token **pident);
Vector *parse_args(Token **ptoken);
Vector *parse_funparams(bool *pvaargs);  // Vector<VarInfo*>, NULL=>old style.
Type *parse_var_def(Type **prawType, int *pstorage, Token **pident);
Expr *parse_const_fixnum(void);
Expr *parse_assign(void);
Expr *parse_expr(void);
Stmt *parse_block(const Token *tok, Scope *scope);
Initializer *parse_initializer(void);

Token *consume(enum TokenKind kind, const char *error);
