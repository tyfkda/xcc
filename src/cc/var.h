// Variables

#pragma once

#include <stdbool.h>

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
  VF_ENUM_MEMBER = 1 << 3,
};

typedef struct VarInfo {
  const Name *name;
  const Type *type;
  int flag;
  union {
    struct {
      Initializer *init;
    } global;
    struct {
      const Name *label;  // For static variable to refer value in global.
    } local;
    struct {
      // For codegen.
      int offset;
    } struct_;
    struct {
      int value;
    } enum_;
  };

  // For codegen.
  VReg *reg;
} VarInfo;

// Variables

void init_global(void);

int var_find(const Vector *vars, const Name *name);  // <VarInfo*>
VarInfo *var_add(Vector *vars, const Name *name, const Type *type, int flag,
                 const Token *ident);  // <VarInfo*>

// Scope

typedef struct Scope {
  struct Scope *parent;
  Vector *vars;  // <VarInfo*>
} Scope;

extern Scope *global_scope;

Scope *new_scope(Scope *parent, Vector *vars);
bool is_global_scope(Scope *scope);
VarInfo *scope_find(Scope *scope, const Name *name, Scope **pscope);
VarInfo *scope_add(Scope *scope, const Token *ident, const Type *type, int flag);
