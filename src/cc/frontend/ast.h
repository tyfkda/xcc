// Abstract Syntax Tree

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>  // int64_t
#include <sys/types.h>  // ssize_t

typedef struct BB BB;
typedef struct Initializer Initializer;
typedef struct MemberInfo MemberInfo;
typedef struct Name Name;
typedef struct Scope Scope;
typedef struct Table Table;
typedef struct Type Type;
typedef struct Vector Vector;

// Num

typedef int64_t  Fixnum;
typedef uint64_t UFixnum;

#ifndef __NO_FLONUM
#ifdef __XCC
typedef long double Flonum;
#else
typedef double Flonum;  // TODO: long double
#endif
#endif

enum StrKind {
  STR_CHAR,
  STR_WIDE,
};

// ================================================

// Line

typedef struct Line {
  const char *filename;
  int lineno;
  const char *buf;
} Line;

// Token

// Token kind
enum TokenKind {
  TK_EOF,            // Represent input end
  TK_ADD,            // +
  TK_SUB,            // -
  TK_MUL,            // *
  TK_DIV,            // /
  TK_MOD,            // %
  TK_AND,            // &
  TK_OR,             // |
  TK_HAT,            // ^
  TK_LT,             // <
  TK_GT,             // >
  TK_NOT,            // !
  TK_LPAR,           // (
  TK_RPAR,           // )
  TK_LBRACE,         // {
  TK_RBRACE,         // }
  TK_LBRACKET,       // [
  TK_RBRACKET,       // ]
  TK_ASSIGN,         // =
  TK_COLON,          // :
  TK_SEMICOL,        // ;
  TK_COMMA,          // ,
  TK_DOT,            // .
  TK_QUESTION,       // ?
  TK_TILDA,          // ~
  TK_INTLIT,         // int literal
  TK_CHARLIT,        // char literal
  TK_LONGLIT,        // long literal
  TK_LLONGLIT,       // long long literal
  TK_UINTLIT,        // unsigned int literal
  TK_UCHARLIT,       // unsigned char literal
  TK_ULONGLIT,       // unsigned long literal
  TK_ULLONGLIT,      // unsigned long long literal
  TK_WCHARLIT,       // wide-char literal
  TK_FLOAT,
  TK_FLOATLIT,       // float literal
  TK_DOUBLE,
  TK_DOUBLELIT,      // double literal
  TK_LDOUBLELIT,     // long double literal
  TK_STR,            // String literal
  TK_IDENT,          // Identifier
  TK_LSHIFT,         // <<
  TK_RSHIFT,         // >>
  TK_EQ,             // ==
  TK_NE,             // !=
  TK_LE,             // <=
  TK_GE,             // >=
  TK_LOGAND,         // &&
  TK_LOGIOR,         // ||
  TK_ARROW,          // ->
  TK_ADD_ASSIGN,     // +=
  TK_SUB_ASSIGN,     // -=
  TK_MUL_ASSIGN,     // *=
  TK_DIV_ASSIGN,     // /=
  TK_MOD_ASSIGN,     // %=
  TK_AND_ASSIGN,     // &=
  TK_OR_ASSIGN,      // |=
  TK_HAT_ASSIGN,     // ^=
  TK_LSHIFT_ASSIGN,  // <<=
  TK_RSHIFT_ASSIGN,  // >>=
  TK_INC,            // ++
  TK_DEC,            // --
  TK_IF,
  TK_ELSE,
  TK_SWITCH,
  TK_CASE,
  TK_DEFAULT,
  TK_DO,
  TK_WHILE,
  TK_FOR,
  TK_BREAK,
  TK_CONTINUE,
  TK_GOTO,
  TK_RETURN,
  TK_VOID,
  TK_CHAR,
  TK_SHORT,
  TK_INT,
  TK_LONG,
  TK_UNSIGNED,
  TK_SIGNED,
  TK_CONST,
  TK_STATIC,
  TK_INLINE,
  TK_EXTERN,
  TK_VOLATILE,
  TK_RESTRICT,
  TK_AUTO,
  TK_REGISTER,
  TK_STRUCT,
  TK_UNION,
  TK_ENUM,
  TK_SIZEOF,
  TK_ALIGNOF,
  TK_TYPEDEF,
  TK_ELLIPSIS,       // ...
  TK_FUNCNAME,
  TK_ASM,
  TK_ATTRIBUTE,

  // For preprocessor.
  PPTK_CONCAT,       // ##
  PPTK_STRINGIFY,    // #
  PPTK_SPACE,        // for macro body
  PPTK_OTHERCHAR,    // Allows illegal character on preprocessor
};

// Token
typedef struct Token {
  enum TokenKind kind;
  Line *line;
  const char *begin;
  const char *end;
  union {
    const Name *ident;
    struct {
      const char *buf;
      size_t len;  // String length, include last '\0'.
      enum StrKind kind;
    } str;
    Fixnum fixnum;
#ifndef __NO_FLONUM
    Flonum flonum;
#endif
  };
} Token;

// ================================================

// Expr

enum ExprKind {
  // Literals
  EX_FIXNUM,  // 1234
  EX_FLONUM,  // 1.23
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

  EX_TERNARY, // a ? b : c
  EX_MEMBER,  // x.member or x->member
  EX_FUNCALL, // f(x, y, ...)
  EX_INLINED, // Inlined function call
  EX_COMPLIT, // Compound literal

  EX_BLOCK,   // Block expression ({...})
};

typedef struct Expr {
  enum ExprKind kind;
  Type *type;
  const Token *token;
  union {
    Fixnum fixnum;
#ifndef __NO_FLONUM
    Flonum flonum;
#endif
    struct {
      const char *buf;
      size_t len;  // String length, include last '\0'.
      enum StrKind kind;
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
      const Name *ident;
      const MemberInfo *info;
    } member;
    struct {
      struct Expr *func;
      Vector *args;  // <Expr*>
    } funcall;
    struct {
      const Name *funcname;
      Vector *args;  // <Expr*>
      struct Stmt *embedded;  // Must be block statement.
    } inlined;
    struct {
      struct Expr *var;
      Vector *inits;  // <Stmt*>
      Initializer *original_init;
    } complit;
    struct Stmt *block;
  };
} Expr;

Expr *new_expr_fixlit(Type *type, const Token *token, const Fixnum fixnum);
#ifndef __NO_FLONUM
Expr *new_expr_flolit(Type *type, const Token *token, double flonum);
#endif
Expr *new_expr_str(const Token *token, const char *str, ssize_t len, enum StrKind kind);  // `len` includes last '\0'.
Expr *new_expr_bop(enum ExprKind kind, Type *type, const Token *token, Expr *lhs, Expr *rhs);
Expr *new_expr_unary(enum ExprKind kind, Type *type, const Token *token, Expr *sub);
Expr *new_expr_deref(const Token *token, Expr *sub);
Expr *new_expr_ternary(const Token *token, Expr *cond, Expr *tval, Expr *fval, Type *type);
Expr *new_expr_variable(const Name *name, Type *type, const Token *token, Scope *scope);
Expr *new_expr_member(const Token *token, Type *type, Expr *target, const Name *ident,
                      const MemberInfo *minfo);
Expr *new_expr_funcall(const Token *token, Expr *func, Type *rettype, Vector *args);
Expr *new_expr_inlined(const Token *token, const Name *name, Type *functype, Vector *args,
                       struct Stmt *embedded);
Expr *new_expr_cast(Type *type, const Token *token, Expr *sub);

Expr *new_expr_complit(Type *type, const Token *token, Expr *var, Vector *inits,
                       Initializer *original);
Expr *new_expr_block(struct Stmt *block);

bool is_const(Expr *expr);
bool is_const_truthy(Expr *expr);
bool is_const_falsy(Expr *expr);
bool is_zero(Expr *expr);
Expr *strip_cast(Expr *expr);

// Initializer

enum InitializerKind {
  IK_SINGLE,  // 123
  IK_MULTI,   // {...}
  IK_DOT,     // .x=123
  IK_ARR,     // [n]=123
};

struct Initializer {
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
      size_t index;
      struct Initializer *value;
    } arr;
  };
};

Initializer *new_initializer(enum InitializerKind kind, const Token *token);

// Statement

enum StmtKind {
  ST_EMPTY,
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
  ST_CASE,  // Include default case.
  ST_GOTO,
  ST_LABEL,
  ST_VARDECL,
  ST_ASM,
};

typedef struct VarDecl {
  const Name *ident;
  struct Stmt *init_stmt;  // Local variable only.
  struct Function *funcproto;
} VarDecl;

VarDecl *new_vardecl(const Name *ident);

enum {
  REACH_STOP = 1 << 0,
  REACH_RETURN = 1 << 1,
};

typedef struct Stmt {
  enum StmtKind kind;
  const Token *token;
  int reach;
  union {
    Expr *expr;
    struct {
      Scope *scope;
      Vector *stmts;
      const Token *rbrace;
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
      struct Stmt *swtch;
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
      struct Stmt *parent;
    } break_;
    struct {
      const Token *label;
    } goto_;
    struct {
      struct Stmt *stmt;
      bool used;
      //
      BB *bb;
    } label;
    struct {
      Expr *val;
    } return_;
    struct {
      Vector *decls;  // <VarDecl*>
    } vardecl;
    struct {
      Expr *str;
      Expr *arg;
    } asm_;
  };
} Stmt;

Stmt *new_stmt(enum StmtKind kind, const Token *token);
Stmt *new_stmt_expr(Expr *e);
Stmt *new_stmt_block(const Token *token, Vector *stmts, Scope *scope, const Token *rbrace);
Stmt *new_stmt_if(const Token *token, Expr *cond, Stmt *tblock, Stmt *fblock);
Stmt *new_stmt_switch(const Token *token, Expr *value);
Stmt *new_stmt_case(const Token *token, Stmt *swtch, Expr *value);
inline Stmt *new_stmt_default(const Token *token, Stmt *swtch)  { return new_stmt_case(token, swtch, NULL); }
Stmt *new_stmt_while(const Token *token, Expr *cond, Stmt *body);
Stmt *new_stmt_for(const Token *token, Expr *pre, Expr *cond, Expr *post, Stmt *body);
Stmt *new_stmt_return(const Token *token, Expr *val);
Stmt *new_stmt_goto(const Token *tok, const Token *label);
Stmt *new_stmt_label(const Token *label, Stmt *follow);
Stmt *new_stmt_vardecl(Vector *decls);
Stmt *new_stmt_asm(const Token *token, Expr *str, Expr *arg);

// ================================================

// Function

typedef struct Function {
  Type *type;
  const Name *name;
  const Vector *params;  // <VarInfo*>

  Vector *scopes;  // NULL => prototype definition.
  Stmt *body_block;  // NULL => Prototype definition.
  Table *label_table;  // <const Name*, Stmt*>
  Vector *gotos;  // <Stmt*>
  void *extra;
  int flag;
} Function;

#define FUNCF_NORETURN        (1 << 0)
#define FUNCF_STACK_MODIFIED  (1 << 1)
#define FUNCF_HAS_FUNCALL     (1 << 2)

Function *new_func(Type *type, const Name *name, const Vector *params, int flag);

// Declaration

enum DeclKind {
  DCL_DEFUN,
  DCL_VARDECL,
  DCL_ASM,
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
    Expr *asmstr;
  };
} Declaration;

Declaration *new_decl_defun(Function *func);
Declaration *new_decl_vardecl(Vector *decls);
Declaration *new_decl_asm(Expr *str);
