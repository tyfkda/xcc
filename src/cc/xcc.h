#pragma once

#include <stdbool.h>
#include <stdint.h>  // uintptr_t, intptr_t
#include <stdio.h>  // FILE

#include "lexer.h"

typedef struct Vector Vector;
typedef struct Map Map;

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

void ensure_struct(Type *type, Token *token);

void dump_type(FILE *fp, const Type *type);

typedef struct Initializer {
  enum { vSingle, vMulti, vDot } type;
  union {
    struct Node *single;
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

Map *struct_map;  // <char*, StructInfo*>
Map *typedef_map;  // <char*, Type*>

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

// Node

enum NodeType {
  ND_CHAR,
  ND_SHORT,
  ND_INT,  // int
  ND_LONG,  // long
  ND_STR,
  ND_VARREF,
  ND_DEFUN,
  ND_FUNCALL,
  ND_BLOCK,
  ND_ADD,  // num + num
  ND_SUB,  // num - num
  ND_MUL,  // num * num
  ND_DIV,  // num / num
  ND_MOD,  // num % num
  ND_NEG,  // -num
  ND_NOT,  // !x
  ND_LSHIFT,  // num << num
  ND_RSHIFT,  // num >> num
  ND_BITAND,
  ND_BITOR,
  ND_BITXOR,
  ND_ASSIGN,
  ND_ASSIGN_WITH,  // +=, etc.
  ND_PREINC,
  ND_PREDEC,
  ND_POSTINC,
  ND_POSTDEC,
  ND_EQ,
  ND_NE,
  ND_LT,
  ND_GT,
  ND_LE,
  ND_GE,
  ND_LOGAND,
  ND_LOGIOR,
  ND_PTRADD,  // ptr + num
  ND_PTRSUB,  // ptr - num
  ND_PTRDIFF,  // ptr - ptr
  ND_REF,
  ND_DEREF,
  ND_MEMBER,  // x.member or x->member
  ND_IF,
  ND_SWITCH,
  ND_WHILE,
  ND_DO_WHILE,
  ND_FOR,
  ND_BREAK,
  ND_CONTINUE,
  ND_RETURN,
  ND_CAST,
  ND_LABEL,  // case, default
  ND_SIZEOF,
};

typedef struct Node {
  enum NodeType type;
  const Type *expType;
  union {
    intptr_t value;
    struct {
      const char *buf;
      size_t len;  // Include last '\0'.
    } str;

    struct {
      struct Node *lhs;
      struct Node *rhs;
    } bop;
    struct {
      struct Node *sub;
    } unary;
    struct {
      const char *ident;
      bool global;
    } varref;
    Defun* defun;
    struct {
      struct Node *func;
      Vector *args;
    } funcall;
    struct {
      Scope *scope;
      Vector *nodes;
    } block;
    struct {
      struct Node *cond;
      struct Node *tblock;
      struct Node *fblock;
    } if_;
    struct {
      struct Node *value;
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
      struct Node *cond;
      struct Node *body;
    } while_;
    struct {
      struct Node *body;
      struct Node *cond;
    } do_while;
    struct {
      struct Node *pre;
      struct Node *cond;
      struct Node *post;
      struct Node *body;
    } for_;
    struct {
      struct Node *val;
    } return_;
    struct {
      struct Node *target;
      //const char *name;
      int index;
    } member;
    struct {
      struct Node *sub;
    } cast;
    struct {
      const Type *type;
    } sizeof_;
  } u;
} Node;

Vector *parse_program(void);

// Variables

int var_find(Vector *vartbl, const char *name);
void var_add(Vector *lvars, const Token *ident, const Type *type, int flag);

Map *global;

GlobalVarInfo *find_global(const char *name);
void define_global(const Type *type, int flag, const Token *ident, Initializer *init);

// Codegen

typedef struct {
  const char *label;
  const void *data;
  size_t size;
} RoData;

extern Vector *loc_vector;

void init_gen(uintptr_t start_address);
void gen(Node *node);
void gen_rodata(void);
void output_code(FILE* fp, size_t filesize);
void add_label(const char *label);
void add_code(const unsigned char* buf, size_t size);
void add_loc_rel8(const char *label, int ofs, int baseofs);
void add_loc_rel32(const char *label, int ofs, int baseofs);
void add_loc_abs64(const char *label, uintptr_t pos);
size_t fixup_locations(size_t *pmemsz);
uintptr_t label_adr(const char *label);
