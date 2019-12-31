// Variables

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t

typedef struct Initializer Initializer;
typedef struct Name Name;
typedef struct Token Token;
typedef struct Type Type;
typedef struct VReg VReg;
typedef struct Vector Vector;

// Varible flags.
enum {
  VF_CONST = 1 << 0,
  VF_STATIC = 1 << 1,
  VF_EXTERN = 1 << 2,
};

typedef struct VarInfo {
  const Name *name;
  const Type *type;
  int flag;
  union {
    struct {  // For global.
      Initializer *init;
    } global;
    struct {  // For local.
      const Name *label;  // For static variable to refer value in global.
    } local;
    struct {
      // For codegen.
      int offset;
    } struct_;
  };

  // For codegen.
  VReg *reg;
} VarInfo;

// Variables

int var_find(Vector *lvars, const Name *name);
VarInfo *var_add(Vector *lvars, const Token *ident, const Type *type, int flag);
Vector *extract_varinfo_types(Vector *params);

extern Vector *gvar_names;

VarInfo *find_global(const Name *name);
VarInfo *define_global(const Type *type, int flag, const Token *ident, const Name *name);

// Scope

typedef struct Scope {
  struct Scope *parent;
  Vector *vars;  // <VarInfo*>

  // For codegen.
  int size;
} Scope;

Scope *new_scope(Scope *parent, Vector *vars);
VarInfo *scope_find(Scope **pscope, const Name *name);
