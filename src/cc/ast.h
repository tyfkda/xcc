// Statement

#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>  // intptr_t

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

// Num

typedef union Num {
  intptr_t ival;
} Num;

// ================================================

// Expr

enum ExprKind {
  // Literals
  EX_NUM,     // 1234
  EX_STR,     // "foobar"

  EX_VARREF,  // foobar

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
  EX_NOT,     // !
  EX_BITNOT,  // ~x
  EX_PREINC,  // ++e
  EX_PREDEC,  // --e
  EX_POSTINC, // e++
  EX_POSTDEC, // e--
  EX_REF,     // &
  EX_DEREF,   // *
  EX_GROUP,   // (x)
  EX_CAST,
  EX_ASSIGN_WITH,  // +=, etc.

  EX_TERNARY, // a ? b : c
  EX_MEMBER,  // x.member or x->member
  EX_SIZEOF,  // sizeof(x)
  EX_FUNCALL, // f(x, y, ...)
};

typedef struct Expr {
  enum ExprKind kind;
  const Type *type;
  const Token *token;
  union {
    Num num;
    struct {
      const char *buf;
      size_t size;  // Include last '\0'.
    } str;
    struct {
      const char *ident;
      Scope *scope;  // NULL = global, non NULL = local
    } varref;
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
      const Token *acctok;  // TK_DOT(.) or TK_ARROW(->)
      const Token *ident;
      int index;
    } member;
    struct {
      const Type *type;  // sizeof(Type), or
      struct Expr *sub;  // sizeof(value)
    } sizeof_;
    struct {
      struct Expr *func;
      Vector *args;  // <Expr*>
    } funcall;
  };
} Expr;

Expr *new_expr_numlit(const Type *type, const Token *token, const Num *num);
Expr *new_expr_str(const Token *token, const char *str, size_t size);
Expr *new_expr_bop(enum ExprKind kind, const Type *type, const Token *token, Expr *lhs, Expr *rhs);
Expr *new_expr_unary(enum ExprKind kind, const Type *type, const Token *token, Expr *sub);
Expr *new_expr_deref(const Token *token, Expr *sub);
Expr *new_expr_ternary(const Token *token, Expr *cond, Expr *tval, Expr *fval, const Type *type);
Expr *new_expr_varref(const char *name, const Type *type, const Token *token);
Expr *new_expr_member(const Token *token, const Type *type, Expr *target, const Token *acctok, const Token *ident, int index);
Expr *new_expr_funcall(const Token *token, Expr *func, Vector *args);
Expr *new_expr_sizeof(const Token *token, const Type *type, Expr *sub);
Expr *new_expr_cast(const Type *type, const Token *token, Expr *sub);

bool is_const(Expr *expr);

// ================================================

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

// Declaration

enum DeclKind {
  DCL_DEFUN,
  DCL_VARDECL,
  DCL_TOPLEVEL,
};

typedef struct Declaration {
  enum DeclKind kind;
  union {
    Defun *defun;
    struct {
      Vector *decls;  // <VarDecl*>
    } vardecl;
    struct {
      Vector *decls;
    } toplevel;
  };
} Declaration;

Declaration *new_decl_defun(Defun *defun);
Declaration *new_decl_vardecl(Vector *decls);
Declaration *new_top_decl(Vector *decls);
