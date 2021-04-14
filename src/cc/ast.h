// Statement

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>  // intptr_t

typedef struct BB BB;
typedef struct BBContainer BBContainer;
typedef struct Name Name;
typedef struct RegAlloc RegAlloc;
typedef struct Scope Scope;
typedef struct Table Table;
typedef struct Token Token;
typedef struct Type Type;
typedef struct VReg VReg;
typedef struct Vector Vector;

// Num

typedef intptr_t  Fixnum;
typedef uintptr_t UFixnum;

// ================================================

// Expr

enum ExprKind {
  // Literals
  EX_FIXNUM,  // 1234
#ifndef __NO_FLONUM
  EX_FLONUM,  // 1.23
#endif
  EX_STR,     // "foobar"

  EX_VAR,     // Variable: foobar

  // Binary operators
  EX_ADD,     // +
  EX_SUB,     // -
  EX_MUL,     // *
  EX_DIV,     // /
  EX_MOD,     // %
  EX_BITAND,  // &
  EX_BITOR,   // |
  EX_BITXOR,  // ^
  EX_LSHIFT,  // <<
  EX_RSHIFT,  // >>
  EX_EQ,      // ==
  EX_NE,      // !=
  EX_LT,      // <
  EX_LE,      // <=
  EX_GE,      // >=
  EX_GT,      // >
  EX_LOGAND,  // &&
  EX_LOGIOR,  // ||
  EX_ASSIGN,  // =
  EX_COMMA,   // head, tail

  // Unary operators
  EX_POS,     // +
  EX_NEG,     // -
  EX_BITNOT,  // ~x
  EX_PREINC,  // ++e
  EX_PREDEC,  // --e
  EX_POSTINC, // e++
  EX_POSTDEC, // e--
  EX_REF,     // &
  EX_DEREF,   // *
  EX_CAST,
  EX_MODIFY,  // +=, etc.

  EX_TERNARY, // a ? b : c
  EX_MEMBER,  // x.member or x->member
  EX_FUNCALL, // f(x, y, ...)
  EX_COMPLIT, // Compound literal
};

typedef struct Expr {
  enum ExprKind kind;
  const Type *type;
  const Token *token;
  union {
    Fixnum fixnum;
#ifndef __NO_FLONUM
    double flonum;
#endif
    struct {
      const char *buf;
      size_t size;  // Include last '\0'.
    } str;
    struct {
      const Name *name;
      Scope *scope;
    } var;
    struct {
      struct Expr *lhs;
      struct Expr *rhs;
    } bop;
    struct {
      struct Expr *sub;
    } unary;
    struct {
      struct Expr *cond;
      struct Expr *tval;
      struct Expr *fval;
    } ternary;
    struct {
      struct Expr *target;
      const Token *ident;
      int index;
    } member;
    struct {
      struct Expr *func;
      Vector *args;  // <Expr*>
    } funcall;
    struct {
      struct Expr *var;
      Vector *inits;  // <Stmt*>
    } complit;
  };
} Expr;

Expr *new_expr_fixlit(const Type *type, const Token *token, const Fixnum fixnum);
Expr *new_expr_flolit(const Type *type, const Token *token, double flonum);
Expr *new_expr_str(const Token *token, const char *str, size_t size);
Expr *new_expr_bop(enum ExprKind kind, const Type *type, const Token *token, Expr *lhs, Expr *rhs);
Expr *new_expr_unary(enum ExprKind kind, const Type *type, const Token *token, Expr *sub);
Expr *new_expr_deref(const Token *token, Expr *sub);
Expr *new_expr_ternary(const Token *token, Expr *cond, Expr *tval, Expr *fval, const Type *type);
Expr *new_expr_variable(const Name *name, const Type *type, const Token *token, Scope *scope);
Expr *new_expr_member(const Token *token, const Type *type, Expr *target, const Token *ident,
                      int index);
Expr *new_expr_funcall(const Token *token, Expr *func, const Type *functype, Vector *args);
Expr *new_expr_cast(const Type *type, const Token *token, Expr *sub);

Expr *new_expr_complit(const Type *type, const Token *token, Expr *var, Vector *inits);

bool is_const(Expr *expr);
bool is_zero(Expr *expr);

// Initializer

enum InitializerKind {
  IK_SINGLE,  // 123
  IK_MULTI,   // {...}
  IK_DOT,     // .x=123
  IK_ARR,     // [n]=123
};

typedef struct Initializer {
  enum InitializerKind kind;
  const Token *token;
  union {
    Expr *single;
    Vector *multi;  // <Initializer*>
    struct {
      const Name *name;
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
};

typedef struct VarDecl {
  const Type *type;
  const Token *ident;
  Initializer *init;
  int storage;
} VarDecl;

VarDecl *new_vardecl(const Type *type, const Token *ident, Initializer *init, int storage);

typedef struct Stmt {
  enum StmtKind kind;
  const Token *token;
  union {
    Expr *expr;
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
      Vector *cases;  // <Stmt*>  contains default, too.
      struct Stmt *default_;
      // codegen
      BB *break_bb;
    } switch_;
    struct {
      Expr *value;  // NULL => default
      //
      BB *bb;
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
  };
} Stmt;

Stmt *new_stmt(enum StmtKind kind, const Token *token);
Stmt *new_stmt_expr(Expr *e);
Stmt *new_stmt_block(const Token *token, Vector *stmts, Scope *scope);
Stmt *new_stmt_if(const Token *token, Expr *cond, Stmt *tblock, Stmt *fblock);
Stmt *new_stmt_switch(const Token *token, Expr *value);
Stmt *new_stmt_case(const Token *token, Expr *value);
Stmt *new_stmt_default(const Token *token);
Stmt *new_stmt_while(const Token *token, Expr *cond, Stmt *body);
Stmt *new_stmt_do_while(Stmt *body, const Token *token, Expr *cond);
Stmt *new_stmt_for(const Token *token, Expr *pre, Expr *cond, Expr *post, Stmt *body);
Stmt *new_stmt_return(const Token *token, Expr *val);
Stmt *new_stmt_goto(const Token *tok, const Token *label);
Stmt *new_stmt_label(const Token *label, Stmt *follow);
Stmt *new_stmt_vardecl(Vector *decls, Vector *inits);
Stmt *new_stmt_asm(const Token *token, Expr *str);

// ================================================

// Function

typedef struct Function {
  const Type *type;
  const Name *name;

  Vector *scopes;  // NULL => prototype definition.
  Vector *stmts;  // NULL => Prototype definition.
  Table *label_table;  // <const Name*, BB*>
  Vector *gotos;  // <Stmt*>

  // For codegen.
  RegAlloc *ra;
  BBContainer *bbcon;
  BB *ret_bb;
  VReg *retval;
} Function;

Function *new_func(const Type *type, const Name *name);

// Declaration

enum DeclKind {
  DCL_DEFUN,
  DCL_VARDECL,
};

typedef struct Declaration {
  enum DeclKind kind;
  union {
    struct {
      Function *func;
    } defun;
    struct {
      Vector *decls;  // <VarDecl*>
    } vardecl;
  };
} Declaration;

Declaration *new_decl_defun(Function *func);
Declaration *new_decl_vardecl(Vector *decls);
