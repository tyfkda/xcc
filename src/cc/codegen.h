#pragma once

#include <stdbool.h>
#include <stdint.h>  // uintptr_t
#include <stdio.h>  // FILE

typedef struct Expr Expr;
typedef struct Node Node;
typedef struct StructInfo StructInfo;
typedef struct Type Type;

#define MAX_REG_ARGS  (6)
#define WORD_SIZE  (8)  /*sizeof(void*)*/

// Public

void gen(Node *node);

// Private

#define PUSH_STACK_POS()  do { stackpos += 8; } while (0)
#define POP_STACK_POS()   do { stackpos -= 8; } while (0)

extern int stackpos;

void init_gen(FILE *fp);
void gen_expr(Expr *expr);
size_t type_size(const Type *type);
void calc_struct_size(StructInfo *sinfo);
void gen_rodata(void);
void add_asm_label(const char *label);
void add_asm2(const char *op, const char *operand1, const char *operand2);
void fixup_locations(void);

void gen_cond_jmp(Expr *cond, bool tf, const char *label);

char *fmt(const char *s, ...);
char *num(intptr_t x);  // x
char *im(intptr_t x);  // $x
char *indirect(const char *reg);
char *offset_indirect(int offset, const char *reg);
char *label_indirect(const char *label, const char *reg);
