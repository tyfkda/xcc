// Expression

#pragma once

#include <stdbool.h>
#include <stdint.h>  // intptr_t
#include <stddef.h>  // size_t

typedef struct Scope Scope;
typedef struct Token Token;
typedef struct Type Type;
typedef struct Vector Vector;

// Num

typedef union {
  intptr_t ival;
} Num;

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

//

const Type *parse_raw_type(int *pflag);
const Type *parse_type_modifier(const Type* type);
const Type *parse_type_suffix(const Type *type);
const Type *parse_full_type(int *pflag, Token **pident);

Expr *new_expr_numlit(const Type *type, const Token *token, const Num *num);
Expr *new_expr_bop(enum ExprKind kind, const Type *type, const Token *token, Expr *lhs, Expr *rhs);
Expr *new_expr_deref(const Token *token, Expr *sub);
Expr *add_expr(const Token *tok, Expr *lhs, Expr *rhs, bool keep_left);
Expr *new_expr_varref(const char *name, const Type *type, const Token *token);
Expr *new_expr_member(const Token *token, const Type *type, Expr *target, const Token *acctok, const Token *ident, int index);
Expr *new_expr_sizeof(const Token *token, const Type *type, Expr *sub);
Expr *new_expr_cast(const Type *type, const Token *token, Expr *sub);
Vector *parse_args(Token **ptoken);
Vector *parse_funparams(bool *pvaargs);
Vector *parse_funparam_types(bool *pvaargs);  // Vector<Type*>
bool parse_var_def(const Type **prawType, const Type** ptype, int *pflag, Token **pident);
Expr *parse_const(void);
Expr *parse_assign(void);
Expr *parse_expr(void);
bool is_const(Expr *expr);
void not_void(const Type *type);
