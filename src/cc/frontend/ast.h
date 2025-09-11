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
typedef struct VarInfo VarInfo;
typedef struct Vector Vector;

// Num

typedef int64_t  Fixnum;
typedef uint64_t UFixnum;

#ifndef __NO_FLONUM
typedef long double Flonum;
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
  TK_INTLIT,         // int literal (ULL, or 'c', L'W')
  TK_FLOAT,
  TK_DOUBLE,
  TK_FLOATLIT,       // float literal (float/double/long double)
  TK_STR,            // String literal
  TK_IDENT,          // Identifier
  TK_LSHIFT,         // <<
  TK_RSHIFT,         // >>
  TK_EQ,             // ==
  TK_NE,             // !=
  TK_LT,             // <
  TK_LE,             // <=
  TK_GE,             // >=
  TK_GT,             // >
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
  TK_BOOL,
  TK_SIZEOF,
  TK_ALIGNOF,
  TK_TYPEDEF,
  TK_ELLIPSIS,       // ...
  TK_ASM,
  TK_ATTRIBUTE,
  TK_NORETURN,
  TK_GENERIC,
  TK_AUTO_TYPE,
  TK_TYPEOF,

  // For preprocessor.
  PPTK_CONCAT,       // ##
  PPTK_STRINGIFY,    // #
  PPTK_SPACE,        // for macro body
  PPTK_OTHERCHAR,    // Allows illegal character on preprocessor
};

#define TKF_LONG_BITS   2
#define TKF_LONG_MASK   ((1 << TKF_LONG_BITS) - 1)
#define TKF_UNSIGNED    (1 << (TKF_LONG_BITS + 0))
#define TKF_CHAR        (1 << (TKF_LONG_BITS + 1))
#define TKF_KIND_SHIFT  (TKF_LONG_BITS + 2)
#define TKF_KIND_PLANE  (0 << TKF_KIND_SHIFT)
#define TKF_KIND_BIN    (1 << TKF_KIND_SHIFT)
#define TKF_KIND_OCT    (2 << TKF_KIND_SHIFT)
#define TKF_KIND_HEX    (3 << TKF_KIND_SHIFT)
#define TKF_KIND_MASK   (3 << TKF_KIND_SHIFT)

// Token
typedef struct Token {
  enum TokenKind kind;
  Line *line;
  const char *begin;
  const char *end;
  union {
    const Name *ident;
    struct {
      char *buf;
      size_t len;  // String length, include last '\0'.
      enum StrKind kind;
    } str;
    struct {
      Fixnum value;
      int flag;
    } fixnum;
#ifndef __NO_FLONUM
    struct {
      Flonum value;
      int kind;  // 0=float, 1=double, 2=long double
    } flonum;
#endif
  };
} Token;

Token *alloc_token(enum TokenKind kind, Line *line, const char *begin, const char *end);
Token *alloc_ident(const Name *name, Line *line, const char *begin, const char *end);

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
      char *buf;
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
      // codegen
      struct FuncallInfo *info;
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

Expr *new_expr(enum ExprKind kind, Type *type, const Token *token);
Expr *new_expr_fixlit(Type *type, const Token *token, Fixnum fixnum);
#ifndef __NO_FLONUM
Expr *new_expr_flolit(Type *type, const Token *token, Flonum flonum);
#endif
Expr *new_expr_bop(enum ExprKind kind, Type *type, const Token *token, Expr *lhs, Expr *rhs);
Expr *new_expr_unary(enum ExprKind kind, Type *type, const Token *token, Expr *sub);
Expr *new_expr_deref(const Token *token, Expr *sub);
Expr *new_expr_ternary(const Token *token, Expr *cond, Expr *tval, Expr *fval, Type *type);
Expr *new_expr_variable(const Name *name, Type *type, const Token *token, Scope *scope);
Expr *new_expr_member(const Token *token, Type *type, Expr *target, const Name *ident,
                      const MemberInfo *minfo);
Expr *new_expr_funcall(const Token *token, const Type *functype, Expr *func, Vector *args);
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
  IK_DOT,     // .x
  IK_BRKT,    // [n]
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
    } bracket;
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
  VarInfo *varinfo;
  struct Stmt *init_stmt;  // Local variable only.
} VarDecl;

VarDecl *new_vardecl(VarInfo *varinfo);

enum {
  REACH_STOP = 1 << 0,
  REACH_RETURN = 1 << 1,
};

typedef struct {
  const Token *constraint;
  Expr *expr;
} AsmArg;

enum {
  ASM_VOLATILE = 1 << 0,
};

typedef struct {
  Vector *templates;  // [const char*, (uintptr_t)index, ...]
  Vector *outputs;  // <AsmArg*>
  Vector *inputs;   // <AsmArg*>
  int flag;
} Asm;

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
      struct Stmt *stmt;
      //
      union {
        BB *bb;
        int block_index;  // for WASM: >=0 if this case has block, < 0 otherwise.
      };
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
    VarDecl *vardecl;
    Asm asm_;
  };
} Stmt;

Stmt *new_stmt(enum StmtKind kind, const Token *token);
Stmt *new_stmt_expr(Expr *e);
Stmt *new_stmt_block(const Token *token, Vector *stmts, Scope *scope, const Token *rbrace);
Stmt *new_stmt_if(const Token *token, Expr *cond, Stmt *tblock, Stmt *fblock);
Stmt *new_stmt_switch(const Token *token, Expr *value);
Stmt *new_stmt_case(const Token *token, Stmt *swtch, Expr *value);
Stmt *new_stmt_while(const Token *token, Expr *cond, Stmt *body);
Stmt *new_stmt_for(const Token *token, Expr *pre, Expr *cond, Expr *post, Stmt *body);
Stmt *new_stmt_return(const Token *token, Expr *val);
Stmt *new_stmt_goto(const Token *tok, const Token *label);
Stmt *new_stmt_label(const Token *label, Stmt *follow);
Stmt *new_stmt_vardecl(VarDecl *vardecl);
Stmt *new_stmt_asm(const Token *token, Vector *template_, Vector *outputs, Vector *inputs,
                   int flag);

// ================================================

// Function

typedef struct Function {
  Type *type;
  const Token *ident;
  const Vector *params;  // <VarInfo*>

  Vector *static_vars;  // Static variable entities: <VarInfo*>
  Vector *scopes;  // NULL => prototype definition.
  Stmt *body_block;  // NULL => Prototype definition.
  Table *label_table;  // <const Name*, Stmt*>
  Vector *gotos;  // <Stmt*>
  void *extra;
  Table *attributes;  // <Vector<Token*>>
  int flag;
} Function;

#define FUNCF_NORETURN        (1 << 0)

Function *new_func(Type *type, const Token *ident, const Vector *params, Table *attributes,
                   int flag);

// Declaration

enum DeclKind {
  DCL_DEFUN,
  DCL_ASM,
};

typedef struct Declaration {
  enum DeclKind kind;
  union {
    struct {
      Function *func;
    } defun;
    struct {
      const Token *token;
      Asm *asm_;
    } asm_;
  };
} Declaration;

Declaration *new_decl_defun(Function *func);
Declaration *new_decl_asm(const Token *token, Asm *asm_);
