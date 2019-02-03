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
void vec_push(Vector *vec, void *elem);

typedef struct {
  Vector *keys;
  Vector *vals;
} Map;

Map *new_map(void);
void map_put(Map *map, char *key, void *val);
void *map_get(Map *map, char *key);

// Token

// Token type value
enum TokenType {
  TK_ADD = '+',
  TK_SUB = '-',
  TK_MUL = '*',
  TK_DIV = '/',
  TK_LPAR = '(',
  TK_RPAR = ')',
  TK_LBRACE = '{',
  TK_RBRACE = '}',
  TK_ASSIGN = '=',
  TK_SEMICOL = ';',
  TK_COMMA = ',',
  TK_NUM = 256,  // Integer token
  TK_IDENT,      // Identifier
  TK_EOF,        // Represent input end
  TK_EQ,  // ==
  TK_NE,  // !=
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

// Node

enum NodeType {
  ND_NUM,     // Number nodes
  ND_IDENT,   // Identifier
  ND_DEFUN,
  ND_FUNCALL,
  ND_ADD,
  ND_SUB,
  ND_MUL,
  ND_DIV,
  ND_ASSIGN,
  ND_EQ,
  ND_NE,
};

typedef struct Node {
  enum NodeType type;
  union {
    struct {
      struct Node *lhs;
      struct Node *rhs;
    } bop;
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
