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
void map_put(Map *map, const char *key, const void *val);
void *map_get(Map *map, const char *key);

// Token

// Token type value
enum TokenType {
  TK_ADD = '+',
  TK_SUB = '-',
  TK_MUL = '*',
  TK_DIV = '/',
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
  TK_IDENT,      // Identifier
  TK_EOF,        // Represent input end
  TK_EQ,  // ==
  TK_NE,  // !=
  TK_IF,
  TK_ELSE,
  TK_WHILE,
  TK_FOR,
  TK_INT,
};

// Token type
typedef struct {
  enum TokenType type;
  const char *input;
  union {
    long val;
    const char *ident;
  };
} Token;

extern Vector *token_vector;

Token *get_token(int pos);
void tokenize(const char *p);

// Type

enum eType {
  TY_VOID,
  TY_INT,
  TY_PTR,
  TY_ARRAY,
};

typedef struct Type {
  enum eType type;
  const struct Type *ptrof;
  size_t array_size;
} Type;

typedef struct {
  const char *name;
  Type *type;
  int offset;
} VarInfo;

// Node

enum NodeType {
  ND_NUM,     // Number nodes
  ND_IDENT,   // Identifier
  ND_DEFUN,
  ND_FUNCALL,
  ND_BLOCK,
  ND_ADD,
  ND_SUB,
  ND_MUL,
  ND_DIV,
  ND_ASSIGN,
  ND_EQ,
  ND_NE,
  ND_REF,
  ND_DEREF,
  ND_IF,
  ND_WHILE,
  ND_FOR,
};

typedef struct Node {
  enum NodeType type;
  const Type *expType;
  union {
    struct {
      struct Node *lhs;
      struct Node *rhs;
    } bop;
    struct {
      struct Node *sub;
    } unary;
    long val;
    const char *ident;
    struct {
      const char *name;
      Vector *lvars;
      Vector *stmts;
      int param_count;
    } defun;
    struct {
      const char *name;
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
      struct Node *pre;
      struct Node *cond;
      struct Node *post;
      struct Node *body;
    } for_;
  };
} Node;

extern Vector *node_vector;

void program(void);

// Variables

int var_find(Vector *vartbl, const char *name);
int var_add(Vector *vartbl, const char *name);

// Codegen

extern Map *label_map;
extern Vector *loc_vector;
extern uintptr_t start_address;

void compile(const char* source);
void output_code(FILE* fp);
void add_label(const char *label);
size_t fixup_locations(void);

// elfutil

void out_elf_header(FILE* fp, uintptr_t entry);
void out_program_header(FILE* fp, uintptr_t offset, uintptr_t vaddr, uintptr_t filesz, uintptr_t memsz);
void put_padding(FILE* fp, uintptr_t prog_start);

// main

void error(const char* fmt, ...);
