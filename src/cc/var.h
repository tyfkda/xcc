// Variables

#pragma once

#include <stdbool.h>

typedef struct Initializer Initializer;
typedef struct Name Name;
typedef struct StructInfo StructInfo;
typedef struct Table Table;
typedef struct Token Token;
typedef struct Type Type;
typedef struct VReg VReg;
typedef struct Vector Vector;

// Varible flags.
enum {
  VF_CONST = 1 << 0,
  VF_STATIC = 1 << 1,
  VF_EXTERN = 1 << 2,
  VF_VOLATILE = 1 << 3,
  VF_ENUM_MEMBER = 1 << 4,
};

typedef struct VarInfo {
  const Name *name;
  const Type *type;
  int flag;
  union {
    struct {
      // For codegen.
      VReg *reg;
    } local;
    struct {
      Initializer *init;
    } global;
    struct {
      struct VarInfo *gvar;  // which points to global(static) variable.
    } static_;
    struct {
      // For codegen.
      int offset;
    } struct_member;
    struct {
      int value;
    } enum_member;
  };
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
  Table *struct_table;  // <StructInfo*>
  Table *typedef_table;  // <Type*>
  Table *enum_table;  // <Type*>
} Scope;

extern Scope *global_scope;

Scope *new_scope(Scope *parent, Vector *vars);
bool is_global_scope(Scope *scope);
VarInfo *scope_find(Scope *scope, const Name *name, Scope **pscope);
VarInfo *scope_add(Scope *scope, const Token *ident, const Type *type, int flag);

StructInfo *find_struct(Scope *scope, const Name *name, Scope **pscope);
void define_struct(Scope *scope, const Name *name, StructInfo *sinfo);

const Type *find_typedef(Scope *scope, const Name *name, Scope **pscope);
bool add_typedef(Scope *scope, const Name *name, const Type *type);

Type *find_enum(Scope *scope, const Name *name);
Type *define_enum(Scope *scope, const Name *name);
