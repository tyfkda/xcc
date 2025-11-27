// Variables

#pragma once

#include <stdbool.h>

typedef struct Declaration Declaration;
typedef struct EnumInfo EnumInfo;
typedef struct Function Function;
typedef struct FrameInfo FrameInfo;
typedef struct Initializer Initializer;
typedef struct Name Name;
typedef struct StructInfo StructInfo;
typedef struct Table Table;
typedef struct Token Token;
typedef struct Type Type;
typedef struct VReg VReg;
typedef struct Vector Vector;

// Storage
enum {
  VS_STATIC = 1 << 0,
  VS_INLINE = 1 << 1,
  VS_EXTERN = 1 << 2,
  VS_ENUM_MEMBER = 1 << 3,
  VS_TYPEDEF = 1 << 4,
  VS_AUTO = 1 << 5,
  VS_REGISTER = 1 << 6,

  VS_REF_TAKEN = 1 << 7,  // `&x` used.
  VS_PARAM = 1 << 8,  // Function parameter
  VS_USED = 1 << 9,  // used.
  VS_STRING = 1 << 10,  // string.
};

typedef struct VarInfo {
  const Token *ident;
  Type *type;
  int storage;
  union {
    struct {
      Initializer *init;
      // For codegen.
      VReg *vreg;
      FrameInfo *frameinfo;
    } local;
    struct {
      union {
        Initializer *init;
        struct {
          Function *func;
          Declaration *funcdecl;
        };
      };
      Vector *referred_globals;  // <VarInfo*>
    } global;
    struct {
      struct VarInfo *svar;  // which points to static variable.
    } static_;
    struct {
      int value;
    } enum_member;
  };
} VarInfo;

// Variables

void init_global(void);

int var_find(const Vector *vars, const Name *name);  // <VarInfo*>
VarInfo *var_add(Vector *vars, const Token *name, Type *type, int storage);  // <VarInfo*>
static inline bool is_local_storage(const VarInfo *varinfo)  { return !(varinfo->storage & (VS_STATIC | VS_EXTERN | VS_ENUM_MEMBER | VS_TYPEDEF)); }

// Scope

enum TypeTagKind {
  TAG_STRUCT,
  TAG_ENUM,
};

enum TagDefineResult {
  TAGRESULT_SUCCESS,
  TAGRESULT_CONFLICT,
  TAGRESULT_DUPLICATED,
};

typedef struct Scope {
  struct Scope *parent;
  Vector *vars;  // <VarInfo*>
  Table *typedef_table;  // <Type*>
  Table *type_tag_table;  // <TypeTag*>
} Scope;

extern Scope *global_scope;
extern Vector *static_vars;  // <VarInfo*>

Scope *new_scope(Scope *parent);
bool is_global_scope(Scope *scope);
VarInfo *scope_find(Scope *scope, const Name *name, Scope **pscope);
VarInfo *scope_add(Scope *scope, const Token *name, Type *type, int storage);

enum TagDefineResult define_type_tag(Scope *scope, const Name *name, enum TypeTagKind kind, void *info);
StructInfo *find_struct(Scope *scope, const Name *name, Scope **pscope);
EnumInfo *find_enum(Scope *scope, const Name *name, Scope **pscope);

Type *find_typedef(Scope *scope, const Name *name, Scope **pscope);
bool add_typedef(Scope *scope, const Name *name, Type *type);
