// Code generation

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t

typedef struct BB BB;
typedef struct Expr Expr;
typedef struct StructInfo StructInfo;
typedef struct Type Type;
typedef struct VReg VReg;
typedef struct VRegType VRegType;
typedef struct Vector Vector;

// Used for returning non-primitive (struct) value.
extern const char RET_VAR_NAME[];

// Public

void gen(Vector *decls);

// Private

VReg *gen_expr(Expr *expr);

void gen_cond_jmp(Expr *cond, bool tf, BB *bb);

void set_curbb(BB *bb);
VReg *add_new_reg(const Type *type, int flag);
VRegType *to_vtype(const Type *type);

bool is_stack_param(const Type *type);

void gen_stmts(Vector *stmts);
