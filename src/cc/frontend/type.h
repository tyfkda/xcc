// Types

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>
#include <stdio.h>
#include <sys/types.h>  // ssize_t

typedef struct Expr Expr;
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
  FX_BOOL,
};

// Flonum

enum FlonumKind {
  FL_FLOAT,
  FL_DOUBLE,
  FL_LDOUBLE,
};

// Type

enum TypeKind {
  TY_VOID,
  TY_FIXNUM,
  TY_FLONUM,
  TY_PTR,
  TY_ARRAY,
  TY_FUNC,
  TY_STRUCT,  // include union
};

// Qualifier
enum {
  TQ_CONST = 1 << 0,
  TQ_VOLATILE = 1 << 1,
  TQ_RESTRICT = 1 << 2,
  TQ_FORSTRLITERAL = 1 << 8,
};

typedef struct MemberInfo {
  const Name *name;
  struct Type *type;
  size_t offset;
#ifndef __NO_BITFIELD
  struct {
    int8_t width;
    uint8_t position;
    uint8_t base_kind;  // FixnumKind
  } bitfield;
#endif
} MemberInfo;

typedef struct StructInfo {
  MemberInfo *members;
  ssize_t size;
  int member_count;
  size_t align;
  bool is_union;
  bool is_flexible;
} StructInfo;

#define LEN_UND  (-1)  // Indicate array length is not specified (= []).
#define LEN_FAM  (-2)  // Indicate this array is flexible array member.
#define LEN_VLA  (-3)  // Indicate this array is variable length array.

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
    struct {
      enum FlonumKind kind;
    } flonum;
    struct {  // Pointer or array.
      struct Type *ptrof;
      ssize_t length;  // of array. Positive value, or LEN_UND, LEN_FAM or LEN_VLA.
#ifndef __NO_VLA
      Expr *vla;  // Variable length array.
      Expr *size_var;
#endif
    } pa;
    struct {
      struct Type *ret;
      const Vector *params;  // <Type*>
      const Vector *param_vars;  // <VarInfo*>: Just for parser, do not use mainly.
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
extern Type tyConstVoid;
extern Type tyVoidPtr;
extern Type tyBool;
extern Type tySize;
extern Type tySSize;
extern Type tyFloat;
extern Type tyDouble;
extern Type tyLDouble;

void set_fixnum_size(enum FixnumKind kind, size_t size, int align);
size_t type_size(const Type *type);
size_t align_size(const Type *type);

inline bool is_fixnum(enum TypeKind kind)  { return kind == TY_FIXNUM; }
inline bool is_flonum(const Type *type)  {
#ifndef __NO_FLONUM
  return type->kind == TY_FLONUM;
#else
  (void)type;
  return false;
#endif
}
inline bool is_bool(const Type *type)  { return type->kind == TY_FIXNUM && type->fixnum.kind == FX_BOOL; }
bool is_number(const Type *type);
bool is_unsigned(const Type *type);
bool is_char_type(const Type *type, /*enum StrKind*/int str_kind);
bool is_void_ptr(const Type *type);
bool is_prim_type(const Type *type);
inline bool ptr_or_array(const Type *type)  { return type->kind == TY_PTR || type->kind == TY_ARRAY; }
Type *get_fixnum_type(enum FixnumKind kind, bool is_unsigned, int qualifier);
Type *get_fixnum_type_from_size(size_t size);
Type *ptrof(Type *type);
Type *arrayof(Type *type, ssize_t length);
Type *array_to_ptr(Type *type);
Type *new_func_type(Type *ret, const Vector *types, bool vaargs);
Type *qualified_type(Type *type, int additional);
Type *clone_type(const Type *type);
Type *get_callee_type(Type *type);

// Struct

StructInfo *create_struct_info(MemberInfo *members, int count, bool is_union, bool is_flexible);
Type *create_struct_type(StructInfo *sinfo, const Name *name, int qualifier);
int find_struct_member(const StructInfo *sinfo, const Name *name);

Type *create_enum_type(const Name *name);

bool same_type_without_qualifier(const Type *type1, const Type *type2, bool ignore_qualifier);
inline bool same_type(const Type *type1, const Type *type2)  { return same_type_without_qualifier(type1, type2, false); }
bool can_cast(const Type *dst, const Type *src, bool zero, bool is_explicit);

//

int print_type(FILE *fp, const Type *type);
