#pragma once

#include <stdint.h>  // uintptr_t
#include <stdio.h>

#ifndef FALSE
#define FALSE  (0)
#endif
#ifndef TRUE
#define TRUE   (1)
#endif

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
  TK_LPAR = '(',
  TK_RPAR = ')',
  TK_LBRACE = '{',
  TK_RBRACE = '}',
  TK_LBRACKET = '[',
  TK_RBRACKET = ']',
  TK_ASSIGN = '=',
  TK_SEMICOL = ';',
  TK_COMMA = ',',
  TK_NUM = 256,  // Integer token
  TK_CHAR,
  TK_STR,        // String literal
  TK_IDENT,      // Identifier
  TK_EOF,        // Represent input end
  TK_EQ,  // ==
  TK_NE,  // !=
  TK_IF,
  TK_ELSE,
  TK_DO,
  TK_WHILE,
  TK_FOR,
  TK_RETURN,
  TK_KWVOID,
  TK_KWINT,
  TK_KWCHAR,
};

// Token type
typedef struct {
  enum TokenType type;
  const char *input;
  union {
    long val;
    const char *ident;
    const char *str;
  };
} Token;

extern Vector *token_vector;

Token *get_token(int pos);
void tokenize(const char *p);

// Type

enum eType {
  TY_VOID,
  TY_INT,
  TY_CHAR,
  TY_PTR,
  TY_ARRAY,
  TY_FUNC,
};

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
  };
} Type;

typedef struct {
  const char *name;
  Type *type;
  int offset;
} VarInfo;

// Node

enum NodeType {
  ND_NUM,     // Number nodes
  ND_CHAR,
  ND_STR,
  ND_VARREF,
  ND_DEFUN,
  ND_FUNCALL,
  ND_BLOCK,
  ND_ADD,
  ND_SUB,
  ND_MUL,
  ND_DIV,
  ND_MOD,
  ND_ASSIGN,
  ND_EQ,
  ND_NE,
  ND_REF,
  ND_DEREF,
  ND_IF,
  ND_WHILE,
  ND_DO_WHILE,
  ND_FOR,
  ND_RETURN,
};

typedef struct Node {
  enum NodeType type;
  const Type *expType;
  union {
    long val;
    const char *str;
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
    struct {
      const Type *rettype;
      const char *name;
      Vector *lvars;
      Vector *stmts;
      int param_count;

      // For codegen.
      const char *ret_label;
    } defun;
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
  };
} Node;

extern Vector *node_vector;

void program(void);

// Variables

int var_find(Vector *vartbl, const char *name);
void var_add(Vector *lvars, const char *name, Type *type);

Map *global;

VarInfo *find_global(const char *name);
void define_global(Type *type, const char *name);

Vector *rodata_vector;

void add_rodata(const char *label, const void *data, size_t size);

// Codegen

extern Map *label_map;
extern Vector *loc_vector;
extern uintptr_t start_address;

void compile(const char* source);
void output_code(FILE* fp);
void add_label(const char *label);
void add_code(const unsigned char* buf, size_t size);
size_t fixup_locations(void);

// elfutil

void out_elf_header(FILE* fp, uintptr_t entry);
void out_program_header(FILE* fp, uintptr_t offset, uintptr_t vaddr, uintptr_t filesz, uintptr_t memsz);
void put_padding(FILE* fp, uintptr_t prog_start);

// main

void error(const char* fmt, ...);
