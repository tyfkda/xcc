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

typedef struct {
  Type *rawType;
  int storage;
  Token *ident;
  Table *attributes;  // <Vector<Token*>>
} ParsedTypeInfo;

typedef Expr *(*BuiltinExprProc)(const Token*);
void add_builtin_expr_ident(const char *str, BuiltinExprProc *proc);

Type *parse_raw_type(ParsedTypeInfo *tinfo);
Type *parse_declarator(Type *rawtype, ParsedTypeInfo *tinfo);
Type *parse_direct_declarator(Type *type, ParsedTypeInfo *tinfo);
Vector *parse_args(Token **ptoken);
Vector *parse_funparams(bool *pvaargs);  // Vector<VarInfo*>, NULL=>old style.
Type *parse_var_def(Type *rawType, ParsedTypeInfo *tinfo);
Expr *parse_const_fixnum(void);
Expr *parse_assign(void);
Expr *parse_expr(void);
Stmt *parse_block(const Token *tok, Scope *scope);
Initializer *parse_initializer(void);
Table *parse_attributes(Table *attributes);  // <Token*>

Token *consume(enum TokenKind kind, const char *error);
