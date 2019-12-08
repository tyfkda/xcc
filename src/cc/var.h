// Variables

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t

typedef struct BB BB;
typedef struct BBContainer BBContainer;
typedef struct Initializer Initializer;
typedef struct Map Map;
typedef struct RegAlloc RegAlloc;
typedef struct Token Token;
typedef struct Type Type;
typedef struct VReg VReg;
typedef struct Vector Vector;

// Varible flags.
enum {
  VF_CONST = 1 << 0,
  VF_STATIC = 1 << 1,
  VF_EXTERN = 1 << 2,
  VF_UNSIGNED = 1 << 3,
};

typedef struct VarInfo {
  const char *name;
  const Type *type;
  int flag;
  union {
    struct {  // For global.
      Initializer *init;
    } global;
    struct {  // For local.
      const char *label;  // For static variable to refer value in global.
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

int var_find(Vector *vartbl, const char *name);
VarInfo *var_add(Vector *lvars, const Token *ident, const Type *type, int flag);
Vector *extract_varinfo_types(Vector *params);

extern Map *gvar_map;

VarInfo *find_global(const char *name);
VarInfo *define_global(const Type *type, int flag, const Token *ident, const char *name);

// Scope

typedef struct Scope {
  struct Scope *parent;
  Vector *vars;  // <VarInfo*>

  // For codegen.
  int size;
} Scope;

Scope *new_scope(Scope *parent, Vector *vars);
VarInfo *scope_find(Scope **pscope, const char *name);

// Function

typedef struct Function {
  const Type *type;
  const char *name;
  Vector *params;  // <VarInfo*>

  Vector *scopes;  // NULL => prototype definition.

  // For codegen.
  RegAlloc *ra;
  BBContainer *bbcon;
  BB *ret_bb;
  size_t frame_size;
  short used_reg_bits;
} Function;

Function *new_func(const Type *type, const char *name, Vector *params);
