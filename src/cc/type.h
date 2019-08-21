#pragma once

#include <stdbool.h>
#include <stdint.h>  // intptr_t
#include <stdio.h>  // FILE
#include <sys/types.h>  // ssize_t

typedef struct Map Map;
typedef struct Token Token;
typedef struct Vector Vector;
typedef struct Initializer Initializer;

// Num

enum NumType {
  NUM_CHAR,  // Small number type should be earlier.
  NUM_SHORT,
  NUM_INT,
  NUM_LONG,
  NUM_ENUM,
};

// Type

enum eType {
  TY_VOID,
  TY_NUM,
  TY_PTR,
  TY_ARRAY,
  TY_FUNC,
  TY_STRUCT,  // include union
};

typedef struct StructInfo {
  Vector *members;  // <VarInfo*>
  ssize_t size;
  int align;
  bool is_union;
} StructInfo;

typedef struct {
  const Token *ident;
  int value;
} EnumMember;

typedef struct Type {
  enum eType type;
  union {
    struct {
      enum NumType type;
      struct {
        const Token *ident;
        Vector *members;  // <EnumMember*>
      } enum_;
    } num;
    struct {  // Pointer or array.
      const struct Type *ptrof;
      size_t length;  // of array. -1 represents length is not specified (= []).
    } pa;
    struct {
      const struct Type *ret;
      Vector *param_types;  // <Type*>
      bool vaargs;
    } func;
    struct {
      const char *name;
      StructInfo *info;
    } struct_;  // and union.
  } u;
} Type;

extern const Type tyChar;
extern const Type tyShort;
extern const Type tyInt;
extern const Type tyLong;
extern const Type tyEnum;
extern const Type tyVoid;
#define tyBool  tyInt
#define tySize  tyLong

bool is_number(enum eType type);
bool is_char_type(const Type *type);
bool is_void_ptr(const Type *type);
bool same_type(const Type *type1, const Type *type2);
Type* ptrof(const Type *type);
const Type *array_to_ptr(const Type *type);
Type* arrayof(const Type *type, size_t length);
Type* new_func_type(const Type *ret, Vector *param_types, bool vaargs);

// Struct

extern Map *struct_map;  // <char*, StructInfo*>
extern Map *enum_map;  // <char*, EnumInfo*>
extern Map *enum_value_map;  // <char*, intptr_t>

StructInfo *find_struct(const char *name);
void define_struct(const char *name, StructInfo *sinfo);

Type *find_enum(const char *name);
Type *define_enum(const Token *ident);
void add_enum_member(Type *type, const Token *ident, int value);
bool find_enum_value(const char *name, intptr_t *output);
