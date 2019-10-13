#pragma once

typedef struct Map Map;
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
      struct Initializer *init;
    } g;
    struct {  // For local.
      const char *label;  // For static variable to refer value in global.
    } l;
  } u;

  // For codegen.
  int offset;
  VReg *reg;
} VarInfo;

// Variables

int var_find(Vector *vartbl, const char *name);
VarInfo *var_add(Vector *lvars, const Token *ident, const Type *type, int flag);

extern Map *gvar_map;

VarInfo *find_global(const char *name);
VarInfo *define_global(const Type *type, int flag, const Token *ident, const char *name);

// Scope

typedef struct Scope {
  struct Scope *parent;
  Vector *vars;

  // For codegen.
  int size;
} Scope;

Scope *new_scope(Scope *parent, Vector *vars);
VarInfo *scope_find(Scope **pscope, const char *name);
