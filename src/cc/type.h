// Types

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>  // intptr_t
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
#ifndef __NO_FLONUM
  TY_FLONUM,
#endif
  TY_PTR,
  TY_ARRAY,
  TY_FUNC,
  TY_STRUCT,  // include union
};

// Qualifier
enum {
  TQ_CONST = 1 << 0,
  TQ_VOLATILE = 1 << 1,
};

typedef struct StructInfo {
  Vector *members;  // <VarInfo*>
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
      const struct Type *ptrof;
      size_t length;  // of array. -1 represents length is not specified (= []).
    } pa;
    struct {
      const struct Type *ret;
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

extern const Type tyChar;
extern const Type tyInt;
extern const Type tyUnsignedChar;
extern const Type tyUnsignedInt;
extern const Type tyEnum;
extern const Type tyVoid;
extern const Type tyVoidPtr;
extern const Type tyBool;
extern const Type tySize;
extern const Type tySSize;
#ifndef __NO_FLONUM
extern const Type tyFloat;
extern const Type tyDouble;
#endif

void set_fixnum_size(enum FixnumKind kind, size_t size, int align);
size_t type_size(const Type *type);
int align_size(const Type *type);

bool is_fixnum(enum TypeKind kind);
bool is_number(const Type *type);
#ifndef __NO_FLONUM
bool is_flonum(const Type *type);
#endif
bool is_char_type(const Type *type);
bool is_void_ptr(const Type *type);
bool ptr_or_array(const Type *type);
const Type *get_fixnum_type(enum FixnumKind kind, bool is_unsigned, int qualifier);
Type *ptrof(const Type *type);
const Type *array_to_ptr(const Type *type);
Type *arrayof(const Type *type, size_t length);
Type *new_func_type(const Type *ret, const Vector *params, const Vector *param_types, bool vaargs);
const Type *qualified_type(const Type *type, int additional);

// Struct

StructInfo *create_struct_info(Vector *members, bool is_union);  // members: <VarInfo*>
Type *create_struct_type(StructInfo *sinfo, const Name *name, int qualifier);

Type *create_enum_type(const Name *name);

bool same_type(const Type *type1, const Type *type2);
bool can_cast(const Type *dst, const Type *src, bool zero, bool is_explicit);

//

void print_type(FILE *fp, const Type *type);
