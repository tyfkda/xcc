// Parser for statement

#pragma once

typedef struct Expr Expr;
typedef struct Stmt Stmt;
typedef struct Vector Vector;

Stmt *new_stmt_expr(Expr *e);
Stmt *new_top_stmt(Vector *stmts);

Vector *parse_program(Vector *stmts);
