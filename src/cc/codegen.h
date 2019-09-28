#pragma once

#include <stdbool.h>
#include <stdint.h>  // uintptr_t
#include <stdio.h>  // FILE

typedef struct BB BB;
typedef struct Expr Expr;
typedef struct Node Node;
typedef struct StructInfo StructInfo;
typedef struct Type Type;
typedef struct VReg VReg;

#define MAX_REG_ARGS  (6)
#define WORD_SIZE  (8)  /*sizeof(void*)*/

// Public

void gen(Node *node);

// Private

#define PUSH_STACK_POS()  do { stackpos += 8; } while (0)
#define POP_STACK_POS()   do { stackpos -= 8; } while (0)

extern int stackpos;

VReg *gen_expr(Expr *expr);
size_t type_size(const Type *type);
int align_size(const Type *type);
void calc_struct_size(StructInfo *sinfo);

void gen_cond_jmp(Expr *cond, bool tf, BB *bb);

char *fmt(const char *s, ...);
char *num(intptr_t x);  // x
char *im(intptr_t x);  // $x
char *indirect(const char *reg);
char *offset_indirect(int offset, const char *reg);
char *label_indirect(const char *label, const char *reg);

void set_curbb(BB *bb);
