#pragma once

#include <stdbool.h>
#include <stdint.h>  // intptr_t
#include <stdio.h>  // FILE

typedef struct Vector Vector;
typedef struct Map Map;
typedef struct Token Token;

// Type

enum eType {
  TY_VOID,
  TY_CHAR,  // Small number type should be earlier.
  TY_SHORT,
  TY_INT,
  TY_LONG,
  TY_ENUM,
  TY_PTR,
  TY_ARRAY,
  TY_FUNC,
  TY_STRUCT,
  TY_UNION,
};

typedef struct {
  Vector *members;  // <VarInfo*>
  bool is_union;
  int size;
  int align;
} StructInfo;

typedef struct Type {
  enum eType type;
  union {
    struct {  // Pointer or array.
      const struct Type *ptrof;
      size_t length;  // of array. -1 represents length is not specified (= []).
    } pa;
    struct {
      const struct Type *ret;
      Vector *params;  // <VarInfo*>
      bool vaargs;
    } func;
    struct {
      const char *name;
      StructInfo *info;
    } struct_;  // and union.
  } u;
} Type;

void ensure_struct(Type *type, const Token *token);
Type* arrayof(const Type *type, size_t length);
bool same_type(const Type *type1, const Type *type2);

void dump_type(FILE *fp, const Type *type);

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

// Varible flags.
enum {
  VF_CONST = 1 << 0,
  VF_STATIC = 1 << 1,
  VF_EXTERN = 1 << 2,
  VF_UNSIGNED = 1 << 3,
};

typedef struct {
  const char *name;
  const Type *type;
  int flag;

  // For codegen.
  int offset;
} VarInfo;

typedef struct {
  const char *name;
  const Type *type;
  int flag;
  Initializer *init;

  // For codegen.
  int offset;
} GlobalVarInfo;

extern Map *struct_map;  // <char*, StructInfo*>
extern Map *typedef_map;  // <char*, Type*>

// Scope

typedef struct Scope {
  struct Scope *parent;
  Vector *vars;

  // For codegen.
  int size;
} Scope;

VarInfo *scope_find(Scope *scope, const char *name);

// Defun

typedef struct {
  const Type *type;
  const char *name;
  Scope *top_scope;
  Vector *stmts;
  Vector *all_scopes;

  // For codegen.
  const char *ret_label;
} Defun;

Scope *enter_scope(Defun *defun, Vector *vars);
void exit_scope(void);
void add_cur_scope(const Token *ident, const Type *type, int flag);

// Expr

enum ExprType {
  // Literals
  EX_CHAR,
  EX_SHORT,
  EX_INT,
  EX_LONG,
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
  union {
    intptr_t value;
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

// Node

enum NodeType {
  ND_EXPR,
  ND_DEFUN,
  ND_BLOCK,
  ND_IF,
  ND_SWITCH,
  ND_WHILE,
  ND_DO_WHILE,
  ND_FOR,
  ND_BREAK,
  ND_CONTINUE,
  ND_RETURN,
  ND_LABEL,  // case, default
};

typedef struct Node {
  enum NodeType type;
  union {
    Expr *expr;
    Defun *defun;
    struct {
      Scope *scope;
      Vector *nodes;
    } block;
    struct {
      struct Expr *cond;
      struct Node *tblock;
      struct Node *fblock;
    } if_;
    struct {
      struct Expr *value;
      struct Node *body;
      Vector *case_values;
      bool has_default;
    } switch_;
    struct {
      enum {lCASE, lDEFAULT} type;
      union {
        int case_value;
      } u;
    } label;
    struct {
      struct Expr *cond;
      struct Node *body;
    } while_;
    struct {
      struct Node *body;
      struct Expr *cond;
    } do_while;
    struct {
      struct Expr *pre;
      struct Expr *cond;
      struct Expr *post;
      struct Node *body;
    } for_;
    struct {
      struct Expr *val;
    } return_;
  } u;
} Node;

Vector *parse_program(void);

// Variables

int var_find(Vector *vartbl, const char *name);
void var_add(Vector *lvars, const Token *ident, const Type *type, int flag);

extern Map *gvar_map;

GlobalVarInfo *find_global(const char *name);
void define_global(const Type *type, int flag, const Token *ident, Initializer *init);

//

bool is_struct_or_union(enum eType type);
void not_void(const Type *type);
bool can_cast(const Type *dst, const Type *src, Expr *src_expr, bool is_explicit);
Type* new_func_type(const Type *ret, const Vector *params, bool vaargs);

const Type *parse_raw_type(int *pflag);
const Type *parse_type_modifier(const Type* type);
const Type *parse_type_suffix(const Type *type);
const Type *parse_full_type(int *pflag, Token **pident);

Expr *new_expr_numlit(enum ExprType exprtype, intptr_t val);
Expr *new_expr_bop(enum ExprType type, const Type *expType, Expr *lhs, Expr *rhs);
Expr *new_expr_deref(Expr *sub);
Expr *add_expr(Token *tok, Expr *lhs, Expr *rhs, bool keep_left);
Expr *new_expr_varref(const char *name, const Type *type, bool global);
Expr *new_expr_member(const Type *valType, Expr *target, const Token *acctok, const Token *ident, int index);
Vector *funparams(bool *pvaargs);
bool parse_var_def(const Type **prawType, const Type** ptype, int *pflag, Token **pident);
Expr *parse_const(void);
Expr *parse_assign(void);
Expr *parse_expr(void);
Expr *analyze_expr(Expr *expr, bool keep_left);
Expr *new_expr_cast(const Type *type, Expr *sub, bool is_explicit);
