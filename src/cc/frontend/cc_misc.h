#pragma once

#include <stdbool.h>
#include <stdint.h>

typedef struct Expr Expr;
typedef struct Initializer Initializer;
typedef struct Type Type;
typedef struct VarInfo VarInfo;

bool is_function_omitted(const VarInfo *funcvi);

typedef struct {
  void (*emit_align)(void *ud, int align);
  void (*emit_number)(void *ud, const Type *type, Expr *var, int64_t offset);  // Flonum is passed as a hexvalue.
  void (*emit_string)(void *ud, Expr *str, size_t size);
} ConstructInitialValueVTable;
void construct_initial_value(const Type *type, const Initializer *init,
                             const ConstructInitialValueVTable *vtable, void *ud);
