// Types

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdio.h>
#include <sys/types.h>  // ssize_t

typedef struct Name Name;
typedef struct Vector Vector;

// Fixnum

enum FixnumKind {
  FX_CHAR,  // Small number kind should be earlier.
  FX_SHORT,
  FX_INT,
  FX_LONG,
  FX_LLONG,
  FX_ENUM,
};

// Flonum

enum FlonumKind {
  FL_FLOAT,
  FL_DOUBLE,
};

// Type

enum TypeKind {
  TY_VOID,
  TY_FIXNUM,
  TY_PTR,
  TY_ARRAY,
  TY_FUNC,
  TY_STRUCT,  // include union
#ifndef __NO_FLONUM
  TY_FLONUM,
#endif
};

// Qualifier
enum {
  TQ_CONST = 1 << 0,
  TQ_VOLATILE = 1 << 1,
};

typedef struct MemberInfo {
  const Name *name;
  struct Type *type;

  // For codegen.
  int offset;
} MemberInfo;

typedef struct StructInfo {
  Vector *members;  // <MemberInfo*>
  ssize_t size;
  int align;
  bool is_union;
} StructInfo;

typedef struct Type {
  enum TypeKind kind;
  int qualifier;
  union {
    struct {
      enum FixnumKind kind;
      bool is_unsigned;
      struct {
        const Name *ident;
      } enum_;
    } fixnum;
#ifndef __NO_FLONUM
    struct {
      enum FlonumKind kind;
    } flonum;
#endif
    struct {  // Pointer or array.
      struct Type *ptrof;
      ssize_t length;  // of array. -1 represents length is not specified (= []).
    } pa;
    struct {
      struct Type *ret;
      const Vector *params;  // <VarInfo*>
      const Vector *param_types;  // <Type*>
      bool vaargs;
    } func;
    struct {
      const Name *name;
      StructInfo *info;
    } struct_;  // and union.
  };
} Type;

extern Type tyChar;
extern Type tyInt;
extern Type tyUnsignedChar;
extern Type tyUnsignedInt;
extern Type tyEnum;
extern Type tyVoid;
extern Type tyVoidPtr;
extern Type tyBool;
extern Type tySize;
extern Type tySSize;
#ifndef __NO_FLONUM
extern Type tyFloat;
extern Type tyDouble;
#define tyLDouble  tyDouble
#endif

void set_fixnum_size(enum FixnumKind kind, size_t size, int align);
size_t type_size(const Type *type);
size_t align_size(const Type *type);

bool is_fixnum(enum TypeKind kind);
bool is_number(const Type *type);
#ifndef __NO_FLONUM
bool is_flonum(const Type *type);
#endif
bool is_char_type(const Type *type);
bool is_void_ptr(const Type *type);
bool ptr_or_array(const Type *type);
Type *get_fixnum_type(enum FixnumKind kind, bool is_unsigned, int qualifier);
Type *ptrof(Type *type);
Type *array_to_ptr(Type *type);
Type *arrayof(Type *type, ssize_t length);
Type *new_func_type(Type *ret, const Vector *params, const Vector *param_types, bool vaargs);
Type *qualified_type(Type *type, int additional);
Type *clone_type(const Type *type);
Type *get_callee_type(Type *type);

// Struct

bool add_struct_member(Vector *members, const Name *name, Type *type);
StructInfo *create_struct_info(Vector *members, bool is_union);  // members: <VarInfo*>
Type *create_struct_type(StructInfo *sinfo, const Name *name, int qualifier);
int find_struct_member(const Vector *members, const Name *name);

Type *create_enum_type(const Name *name);

bool same_type(const Type *type1, const Type *type2);
bool can_cast(const Type *dst, const Type *src, bool zero, bool is_explicit);

//

void print_type(FILE *fp, const Type *type);
