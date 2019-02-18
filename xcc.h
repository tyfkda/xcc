#pragma once

#include <stdint.h>  // uintptr_t
#include <stdio.h>

// Container

typedef struct {
  void **data;
  int capacity;
  int len;
} Vector;

Vector *new_vector(void);
void vec_push(Vector *vec, const void *elem);

typedef struct {
  Vector *keys;
  Vector *vals;
} Map;

Map *new_map(void);
int map_count(Map *map);
void map_put(Map *map, const char *key, const void *val);
void *map_get(Map *map, const char *key);

// Token

// Token type value
enum TokenType {
  TK_ADD = '+',
  TK_SUB = '-',
  TK_MUL = '*',
  TK_DIV = '/',
  TK_MOD = '%',
  TK_AMP = '&',
  TK_LT = '<',
  TK_GT = '>',
  TK_NOT = '!',
  TK_LPAR = '(',
  TK_RPAR = ')',
  TK_LBRACE = '{',
  TK_RBRACE = '}',
  TK_LBRACKET = '[',
  TK_RBRACKET = ']',
  TK_ASSIGN = '=',
  TK_SEMICOL = ';',
  TK_COMMA = ',',
  TK_DOT = '.',
  TK_INTLIT = 256,  // int literal
  TK_CHARLIT,  // char literal
  TK_LONGLIT,  // long literal
  TK_STR,        // String literal
  TK_IDENT,      // Identifier
  TK_EOF,        // Represent input end
  TK_EQ,  // ==
  TK_NE,  // !=
  TK_LE,  // <=
  TK_GE,  // >=
  TK_LOGAND,  // &&
  TK_LOGIOR,  // ||
  TK_ARROW,  // ->
  TK_ADD_ASSIGN,  // +=
  TK_SUB_ASSIGN,  // -=
  TK_MUL_ASSIGN,  // *=
  TK_DIV_ASSIGN,  // /=
  TK_MOD_ASSIGN,  // %=
  TK_INC,
  TK_DEC,
  TK_IF,
  TK_ELSE,
  TK_DO,
  TK_WHILE,
  TK_FOR,
  TK_BREAK,
  TK_CONTINUE,
  TK_RETURN,
  TK_KWVOID,
  TK_KWCHAR,
  TK_KWINT,
  TK_KWLONG,
  TK_STRUCT,
};

// Token type
typedef struct {
  enum TokenType type;
  const char *input;
  union {
    const char *ident;
    const char *str;
    long longval;
    int intval;
    char charval;
  };
} Token;

void init_lexer(FILE *fp);
Token *consume(enum TokenType type);
const char *current_line(void);

// Type

enum eType {
  TY_VOID,
  TY_CHAR,  // Small number type should be earlier.
  TY_INT,
  TY_LONG,
  TY_PTR,
  TY_ARRAY,
  TY_FUNC,
  TY_STRUCT,
};

typedef struct {
  Vector *members;  // <VarInfo*>
  int size;
  int align;
} StructInfo;

typedef struct Type {
  enum eType type;
  union {
    struct {
      const struct Type *ptrof;
      size_t array_size;
    };
    struct {
      const struct Type *ret;
      Vector *params;
    } func;
    StructInfo *struct_;
  };
} Type;

typedef struct {
  const char *name;
  const Type *type;

  // For codegen.
  int offset;
} VarInfo;

Map *struct_map;

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
  const Type *rettype;
  const char *name;
  Scope *top_scope;  // = params
  Vector *stmts;
  Vector *all_scopes;

  // For codegen.
  const char *ret_label;
} Defun;

// Node

enum NodeType {
  ND_INT,  // int
  ND_CHAR,
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
  ND_WHILE,
  ND_DO_WHILE,
  ND_FOR,
  ND_BREAK,
  ND_CONTINUE,
  ND_RETURN,
  ND_CAST,
};

typedef struct Node {
  enum NodeType type;
  const Type *expType;
  union {
    const char *str;
    long longval;
    int intval;
    char charval;

    struct {
      struct Node *lhs;
      struct Node *rhs;
    } bop;
    struct {
      struct Node *sub;
    } unary;
    struct {
      const char *ident;
      int global;
    } varref;
    Defun* defun;
    struct {
      struct Node *func;
      Vector *args;
    } funcall;
    struct {
      Vector *nodes;
    } block;
    struct {
      struct Node *cond;
      struct Node *tblock;
      struct Node *fblock;
    } if_;
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
      const char *name;
    } member;
    struct {
      struct Node *sub;
    } cast;
  };
} Node;

Vector *parse_program(void);

// Variables

int var_find(Vector *vartbl, const char *name);
void var_add(Vector *lvars, const char *name, const Type *type);

Map *global;

VarInfo *find_global(const char *name);
void define_global(const Type *type, const char *name);

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
void output_code(FILE* fp);
void add_label(const char *label);
void add_code(const unsigned char* buf, size_t size);
void add_loc_rel32(const char *label, int ofs, int baseofs);
size_t fixup_locations(void);
uintptr_t label_adr(const char *label);

// elfutil

void out_elf_header(FILE* fp, uintptr_t entry);
void out_program_header(FILE* fp, uintptr_t offset, uintptr_t vaddr, uintptr_t filesz, uintptr_t memsz);
void put_padding(FILE* fp, uintptr_t prog_start);

// main

void error(const char* fmt, ...);
