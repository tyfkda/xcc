#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>
#include <stdio.h>
#include <sys/types.h>  // ssize_t

typedef struct DataStorage DataStorage;
typedef struct Expr Expr;
typedef struct Function Function;
typedef struct Name Name;
typedef struct Table Table;
typedef struct Token Token;
typedef struct Type Type;
typedef struct VarInfo VarInfo;
typedef struct Vector Vector;

#define I32_SIZE  (4)  //sizeof(int32_t)

extern const char VA_ARGS_NAME[];

extern Table builtin_function_table;
extern Vector *functypes;  // <DataStorage*>
extern Table unresolved_gvar_table;
extern uint32_t data_end_address;

#define FF_REFERRED  (1 << 0)
#define FF_INDIRECT  (1 << 1)
#define FF_INLINING  (1 << 2)

typedef struct {
  Function *func;
  VarInfo *varinfo;
  const Name *bpname;
  uint32_t index;
  int flag;
  uint32_t type_index;
  uint32_t indirect_index;
} FuncInfo;

typedef struct {
  VarInfo *varinfo;
  bool is_export;
  union {
    struct {
      uint32_t index;
    } prim;
    struct {
      uint32_t address;
    } non_prim;
  };
} GVarInfo;

// traverse
uint32_t traverse_ast(Vector *decls, Vector *exports, uint32_t stack_size);

bool is_stack_param(const Type *type);
GVarInfo *get_gvar_info_from_name(const Name *name);
GVarInfo *get_gvar_info(Expr *expr);
int getsert_func_type_index(const Type *type, bool reg);
inline int get_func_type_index(const Type *type)  { return getsert_func_type_index(type, false); }

void modify_ast_for_setjmp(int n);

// gen_wasm
void gen(Vector *decls);
void gen_expr(Expr *expr, bool needval);
void gen_expr_stmt(Expr *expr);

enum BuiltinFunctionPhase {
  BFP_TRAVERSE,
  BFP_GEN,
};
typedef void (*BuiltinFunctionProc)(Expr *expr, enum BuiltinFunctionPhase phase);
void add_builtin_function(const char *str, Type *type, BuiltinFunctionProc *proc,
                          bool add_to_scope);

#define ADD_CODE(...)  do { unsigned char buf[] = {__VA_ARGS__}; add_code(buf, sizeof(buf)); } while (0)
void add_code(const unsigned char* buf, size_t size);

void install_builtins(void);

// emit_wasm
void emit_wasm(FILE *ofp, Vector *exports, const char *import_module_name, uint32_t address_bottom);

// wcc_util
extern const char SP_NAME[];

extern bool verbose;
extern Table func_info_table;
extern Table gvar_info_table;
extern Table indirect_function_table;
extern Vector *tags;  // <int>

#define VERBOSES(str)  do { if (verbose) printf("%s", str); } while (0)
#define VERBOSE(fmt, ...)  do { if (verbose) printf(fmt, __VA_ARGS__); } while (0)

uint32_t get_indirect_function_index(const Name *name);
GVarInfo *register_gvar_info(const Name *name, VarInfo *varinfo);
GVarInfo *get_gvar_info_from_name(const Name *name);
int getsert_func_type(unsigned char *buf, size_t size, bool reg);
int getsert_tag(int typeindex);

void write_wasm_header(FILE *ofp);

typedef struct DataStorage {
  Vector *chunk_stack;
  unsigned char *buf;
  size_t capacity;
  size_t len;
} DataStorage;

void data_release(DataStorage *data);
void data_init(DataStorage *data);
void data_reserve(DataStorage *data, size_t capacity);
void data_insert(DataStorage *data, ssize_t pos, const unsigned char *buf, size_t size);
void data_append(DataStorage *data, const unsigned char *buf, size_t size);
void data_push(DataStorage *data, unsigned char c);
void data_concat(DataStorage *dst, DataStorage *src);
void data_leb128(DataStorage *data, ssize_t pos, int64_t val);
void data_uleb128(DataStorage *data, ssize_t pos, uint64_t val);
void data_string(DataStorage *data, const void *str, size_t len);
void data_open_chunk(DataStorage *data);
void data_close_chunk(DataStorage *data, ssize_t num);

typedef struct FuncExtra {
  Vector *funcall_results;  // [0]=Expr*, [1]=VarInfo*
  DataStorage *code;
  int setjmp_count;
} FuncExtra;

Expr *get_sp_var(void);
unsigned char to_wtype(const Type *type);
