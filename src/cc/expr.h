#pragma once

#include <stdbool.h>
#include <stdint.h>  // intptr_t
#include <stdio.h>  // FILE
#include <sys/types.h>  // ssize_t

typedef struct Map Map;
typedef struct Scope Scope;
typedef struct Token Token;
typedef struct Type Type;
typedef struct VarInfo VarInfo;
typedef struct Vector Vector;
enum eType;

// Num

typedef union {
  intptr_t ival;
} Num;

// Type

void ensure_struct(Type *type, const Token *token);

// Initializer

typedef struct Initializer {
  enum { vSingle, vMulti, vDot } type;  // vSingle: 123, vMulti: {...}, vDot: .x=123
  union {
    struct Expr *single;
    Vector *multi;  // <Initializer*>
    struct {
      const char *name;
      struct Initializer *value;
    } dot;
  } u;
} Initializer;

Initializer **flatten_initializer(const Type *type, Initializer *init);

extern Map *typedef_map;  // <char*, Type*>

// Defun

typedef struct Defun {
  const Type *type;
  const char *name;
  Vector *params;  // <VarInfo*>
  Scope *top_scope;
  Vector *stmts;
  Vector *all_scopes;
  Map *labels;
  Vector *gotos;

  // For codegen.
  const char *ret_label;
} Defun;

Scope *enter_scope(Defun *defun, Vector *vars);
void exit_scope(void);
VarInfo *add_cur_scope(const Token *ident, const Type *type, int flag);

extern Scope *curscope;

// Expr

enum ExprType {
  // Literals
  EX_NUM,
  EX_STR,

  EX_VARREF,

  // Binary operators
  EX_ADD,  // num + num
  EX_SUB,  // num - num
  EX_MUL,  // num * num
  EX_DIV,  // num / num
  EX_MOD,  // num % num
  EX_BITAND,
  EX_BITOR,
  EX_BITXOR,
  EX_LSHIFT,  // num << num
  EX_RSHIFT,  // num >> num
  EX_EQ,
  EX_NE,
  EX_LT,
  EX_GT,
  EX_LE,
  EX_GE,
  EX_LOGAND,
  EX_LOGIOR,
  EX_ASSIGN,

  // Unary operators
  EX_POS,  // +num
  EX_NEG,  // -num
  EX_NOT,  // !x
  EX_PREINC,
  EX_PREDEC,
  EX_POSTINC,
  EX_POSTDEC,
  EX_REF,    // &x
  EX_DEREF,  // *x
  EX_CAST,
  EX_ASSIGN_WITH,  // +=, etc.

  EX_TERNARY,

  EX_MEMBER,  // x.member or x->member
  EX_SIZEOF,
  EX_FUNCALL,
  EX_COMMA,
};

typedef struct Expr {
  enum ExprType type;
  const Type *valType;
  const Token *token;
  union {
    Num num;
    struct {
      const char *buf;
      size_t size;  // Include last '\0'.
    } str;
    struct {
      const char *ident;
      bool global;
    } varref;

    struct {
      struct Expr *lhs;
      struct Expr *rhs;
    } bop;
    struct {
      struct Expr *sub;
    } unary;
    struct {
      struct Expr *sub;
    } cast;
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
      const Type *type;
      struct Expr *sub;
    } sizeof_;
    struct {
      struct Expr *func;
      Vector *args;  // <Expr*>
    } funcall;
    struct {
      Vector *list;  // <Expr*>
    } comma;
  } u;
} Expr;

//

void not_void(const Type *type);
bool can_cast(const Type *dst, const Type *src, Expr *src_expr, bool is_explicit);

const Type *parse_raw_type(int *pflag);
const Type *parse_type_modifier(const Type* type);
const Type *parse_type_suffix(const Type *type);
const Type *parse_full_type(int *pflag, Token **pident);

Expr *new_expr(enum ExprType type, const Type *valType, const Token *token);
Expr *new_expr_numlit(const Type *type, const Token *token, const Num *num);
Expr *new_expr_bop(enum ExprType type, const Type *valType, const Token *token, Expr *lhs, Expr *rhs);
Expr *new_expr_deref(const Token *token, Expr *sub);
Expr *add_expr(const Token *tok, Expr *lhs, Expr *rhs, bool keep_left);
Expr *new_expr_varref(const char *name, const Type *type, bool global, const Token *token);
Expr *new_expr_member(const Token *token, const Type *valType, Expr *target, const Token *acctok, const Token *ident, int index);
Expr *new_expr_sizeof(const Token *token, const Type *type, Expr *sub);
Vector *funparams(bool *pvaargs);
bool parse_var_def(const Type **prawType, const Type** ptype, int *pflag, Token **pident);
Expr *parse_const(void);
Expr *parse_assign(void);
Expr *parse_expr(void);
Expr *analyze_expr(Expr *expr, bool keep_left);
Expr *new_expr_cast(const Type *type, const Token *token, Expr *sub, bool is_explicit);
bool check_cast(const Type *dst, const Type *src, Expr *src_expr, bool is_explicit);
bool is_const(Expr *expr);
