// Code generation

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t

typedef struct BB BB;
typedef struct Expr Expr;
typedef struct Function Function;
typedef struct Stmt Stmt;
typedef struct StructInfo StructInfo;
typedef struct Type Type;
typedef struct VReg VReg;
typedef struct VRegType VRegType;
typedef struct VarInfo VarInfo;
typedef struct Vector Vector;

// Public

void gen(Vector *decls);

// Private

VReg *gen_expr(Expr *expr);

void gen_cond_jmp(Expr *cond, bool tf, BB *bb);

void set_curbb(BB *bb);
VReg *add_new_reg(const Type *type, int vflag);
VRegType to_vtype(const Type *type);
int to_vflag(const Type *type);

bool is_stack_param(const Type *type);

void gen_stmt(struct Stmt *stmt);
void gen_stmts(Vector *stmts);

typedef VReg *(*BuiltinFunctionProc)(Expr *expr);
void add_builtin_function(const char *str, Type *type, BuiltinFunctionProc *proc, bool add_to_scope);

void gen_clear_local_var(const VarInfo *varinfo);
void gen_memcpy(const Type *type, VReg *dst, VReg *src);

typedef struct {
  const Type *type;
  VReg *vreg;
  int index;
} RegParamInfo;

void enumerate_register_params(
    Function *func, RegParamInfo iargs[], int max_ireg, RegParamInfo fargs[], int max_freg,
    int *piarg_count, int *pfarg_count);
