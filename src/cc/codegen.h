#pragma once

#include <stdbool.h>
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

void set_curbb(BB *bb);
