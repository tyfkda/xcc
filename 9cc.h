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
  TK_NUM = 256,  // Integer token
  TK_IDENT,      // Identifier
  TK_EOF,        // Represent input end
};

// Token type
typedef struct {
  int ty;
  const char *input;
  union {
    long val;
    char ident;
  };
} Token;

extern Vector *token_vector;

Token *get_token(int pos);
void tokenize(const char *p);

// Node

enum {
  ND_NUM = 256,     // Number nodes
  ND_IDENT,         // Identifier
};

typedef struct Node {
  int ty;
  union {
    struct {
      struct Node *lhs;
      struct Node *rhs;
    } bop;
    long val;
    char name;
  };
} Node;

extern Vector *node_vector;

void program(void);

// Codegen

size_t compile(const char* source);
void output_code(FILE* fp);

// elfutil

void out_elf_header(FILE* fp, uintptr_t entry);
void out_program_header(FILE* fp, uintptr_t offset, uintptr_t vaddr, uintptr_t filesz, uintptr_t memsz);
void put_padding(FILE* fp, uintptr_t prog_start);

// main

void error(const char* fmt, ...);
