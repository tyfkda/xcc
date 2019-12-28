// Statement

#pragma once

#include <stdbool.h>
#include <stddef.h>

typedef struct BB BB;
typedef struct BBContainer BBContainer;
typedef struct Function Function;
typedef struct Expr Expr;
typedef struct Map Map;
typedef struct RegAlloc RegAlloc;
typedef struct Scope Scope;
typedef struct Token Token;
typedef struct Type Type;
typedef struct Vector Vector;

// Function

typedef struct Function {
  const Type *type;
  const char *name;
  Vector *params;  // <VarInfo*>

  Vector *scopes;  // NULL => prototype definition.

  // For codegen.
  RegAlloc *ra;
  BBContainer *bbcon;
  BB *ret_bb;
  size_t frame_size;
  short used_reg_bits;
} Function;

// Defun

typedef struct Defun {
  Function *func;

  Vector *stmts;  // NULL => Prototype definition.

  Map *label_map;  // <const char*, BB*>
  Vector *gotos;

  int flag;
} Defun;

// Initializer

typedef struct Initializer {
  enum { vSingle, vMulti, vDot, vArr } kind;  // vSingle: 123, vMulti: {...}, vDot: .x=123, vArr: [n]=123
  const Token *token;
  union {
    Expr *single;
    Vector *multi;  // <Initializer*>
    struct {
      const char *name;
      struct Initializer *value;
    } dot;
    struct {
      Expr *index;
      struct Initializer *value;
    } arr;
  };
} Initializer;

// Statement

enum StmtKind {
  ST_EXPR,
  ST_DEFUN,
  ST_BLOCK,
  ST_IF,
  ST_SWITCH,
  ST_WHILE,
  ST_DO_WHILE,
  ST_FOR,
  ST_BREAK,
  ST_CONTINUE,
  ST_RETURN,
  ST_CASE,
  ST_DEFAULT,
  ST_GOTO,
  ST_LABEL,
  ST_VARDECL,
  ST_ASM,
  ST_TOPLEVEL,
};

typedef struct VarDecl {
  const Type *type;
  const Token *ident;
  Initializer *init;
  int flag;
} VarDecl;

typedef struct Stmt {
  enum StmtKind kind;
  const Token *token;
  union {
    Expr *expr;
    Defun *defun;
    struct {
      Scope *scope;
      Vector *stmts;
    } block;
    struct {
      Expr *cond;
      struct Stmt *tblock;
      struct Stmt *fblock;
    } if_;
    struct {
      Expr *value;
      struct Stmt *body;
      Vector *case_values;  // <intptr_t>
      bool has_default;
    } switch_;
    struct {
      Expr *value;
    } case_;
    struct {
      Expr *cond;
      struct Stmt *body;
    } while_;
    struct {
      Expr *pre;
      Expr *cond;
      Expr *post;
      struct Stmt *body;
    } for_;
    struct {
      const Token *label;
    } goto_;
    struct {
      // const Token *label;
      struct Stmt *stmt;
    } label;
    struct {
      Expr *val;
    } return_;
    struct {
      Vector *decls;  // <VarDecl*>
      Vector *inits;  // <Stmt*>
    } vardecl;
    struct {
      Expr *str;
    } asm_;
    struct {
      Vector *stmts;
    } toplevel;
  };
} Stmt;

Stmt *new_stmt(enum StmtKind kind, const Token *token);
Stmt *new_stmt_expr(Expr *e);
Stmt *new_stmt_block(const Token *token, Vector *stmts);
Stmt *new_stmt_if(const Token *token, Expr *cond, Stmt *tblock, Stmt *fblock);
Stmt *new_stmt_switch(const Token *token, Expr *value);
Stmt *new_stmt_case(const Token *token, Expr *value);
Stmt *new_stmt_default(const Token *token);
Stmt *new_stmt_while(const Token *token, Expr *cond, Stmt *body);
Stmt *new_stmt_do_while(Stmt *body, const Token *token, Expr *cond);
Stmt *new_stmt_for(const Token *token, Expr *pre, Expr *cond, Expr *post, Stmt *body);
Stmt *new_stmt_return(const Token *token, Expr *val);
Stmt *new_stmt_goto(const Token *label);
Stmt *new_stmt_label(const Token *label, Stmt *follow);
Stmt *new_stmt_vardecl(Vector *decls);
Stmt *new_stmt_asm(const Token *token, Expr *str);
Stmt *new_stmt_defun(Defun *defun);
Stmt *new_top_stmt(Vector *stmts);
