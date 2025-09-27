// Code generation

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t

#include "ir.h"  // enum VRegSize

typedef struct BB BB;
typedef struct Expr Expr;
typedef struct Function Function;
typedef struct RegAlloc RegAlloc;
typedef struct Stmt Stmt;
typedef struct Type Type;
typedef struct VReg VReg;
typedef struct VarInfo VarInfo;
typedef struct Vector Vector;

// Public

void gen(Vector *decls);

// Private

VReg *gen_expr(Expr *expr);

void gen_cond_jmp(Expr *cond, BB *tbb, BB *fbb);

void set_curbb(BB *bb);
VReg *add_new_vreg_with_storage(const Type *type, int storage);
static inline VReg *add_new_vreg(const Type *type)  { return add_new_vreg_with_storage(type, 0); }
enum VRegSize to_vsize(const Type *type);
int to_vflag_with_storage(const Type *type, int storage);
static inline int to_vflag(const Type *type)  { return to_vflag_with_storage(type, 0); }

bool is_stack_param(const Type *type);

void gen_stmt(struct Stmt *stmt);
VReg *gen_stmts(Vector *stmts);
VReg *gen_block(Stmt *stmt);

typedef VReg *(*BuiltinFunctionProc)(Expr *expr);
void add_builtin_function(const char *str, Type *type, BuiltinFunctionProc *proc,
                          bool add_to_scope);

void gen_clear_local_var(const VarInfo *varinfo);
void gen_memcpy(const Type *type, VReg *dst, VReg *src);

typedef struct {
  const VarInfo *varinfo;
  VReg *vreg;
  int index;
} RegParamInfo;

int enumerate_register_params(Function *func, const int max_reg[2], RegParamInfo *args);

bool gen_defun(Function *func);
void prepare_register_allocation(Function *func);
void map_virtual_to_physical_registers(RegAlloc *ra);
void detect_living_registers(RegAlloc *ra, BBContainer *bbcon);
void alloc_stack_variables_onto_stack_frame(Function *func);
