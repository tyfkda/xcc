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
typedef struct Scope Scope;
typedef struct Table Table;
typedef struct Token Token;
typedef struct Type Type;
typedef struct VarInfo VarInfo;
typedef struct Vector Vector;

#define DEFAULT_STACK_SIZE  (8 * 1024)

#define I32_SIZE  (4)  //sizeof(int32_t)

#define INDIRECT_FUNCTION_TABLE_START_INDEX  (1)  // To avoid 0, which is used as NULL.

extern const char VA_ARGS_NAME[];

extern Table builtin_function_table;
extern Vector *functypes;  // <DataStorage*>

#define FF_REFERRED  (1 << 0)
#define FF_INDIRECT  (1 << 1)
#define FF_INLINING  (1 << 2)
#define FF_WEAK      (1 << 3)
#define FF_STACK_MODIFIED  (1 << 4)

typedef struct {
  Function *func;
  VarInfo *varinfo;
  const Name *module_name;
  const Name *func_name;
  const Name *bpname;
  const Name *lspname;
  uint32_t index;  // also represents symbol_index (because functions are put first in symbol table.)
  int flag;
  uint32_t type_index;
  uint32_t indirect_index;
  size_t stack_work_size;  // Work size for funcall, etc.
} FuncInfo;

#define GVF_EXPORT      (1 << 0)
#define GVF_UNRESOLVED  (1 << 1)

typedef struct {
  VarInfo *varinfo;
  int flag;
  union {
    struct {
      uint32_t index;
    } prim;
    struct {
      uint32_t address;
    } non_prim;
  };

  // For object format.
  uint32_t item_index;    // global, or data
  uint32_t symbol_index;  // index in symbol table.
} GVarInfo;

typedef struct {
  const Name *name;
  int typeindex;
  uint32_t index;
  uint32_t symbol_index;
} TagInfo;

// traverse
void traverse_ast(Vector *decls);

bool is_stack_param(const Type *type);
GVarInfo *get_gvar_info_from_name(const Name *name);
GVarInfo *get_gvar_info(Expr *expr);
int getsert_func_type_index(const Type *type, bool reg);
static inline int get_func_type_index(const Type *type)  { return getsert_func_type_index(type, false); }

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
typedef struct {
  FILE *ofp;
  const char *import_module_name;
  Vector *exports;
  Vector *data_segments;
  uint32_t section_index;
  uint32_t function_count;
  uint32_t code_section_index;
  uint32_t data_section_index;
  uint32_t import_global_count;
} EmitWasm;

void emit_wasm(FILE *ofp, const char *import_module_name, Vector *exports);

void emit_type_section(EmitWasm *ew);
void emit_tag_section(EmitWasm *ew);

// wcc_util
enum OutType {
  OutPreprocess,
  OutObject,
  OutExecutable,
};

#define CUF_LINEAR_MEMORY  (1 << 0)
#define CUF_USE_SP         (1 << 1)
#define CUF_INDIRECT_CALL  (1 << 2)

extern const char SP_NAME[];
extern const char BREAK_ADDRESS_NAME[];

extern bool verbose;
extern Table func_info_table;
extern Table gvar_info_table;
extern Table indirect_function_table;  // <FuncInfo*>
extern Vector *tags;  // <TagInfo*>
extern Vector *init_funcs;  // <Function*>
extern int compile_unit_flag;

#define VERBOSES(str)  do { if (verbose) printf("%s", str); } while (0)
#define VERBOSE(fmt, ...)  do { if (verbose) printf(fmt, __VA_ARGS__); } while (0)

uint32_t get_indirect_function_index(const Name *name);
GVarInfo *register_gvar_info(const Name *name, VarInfo *varinfo);
GVarInfo *get_gvar_info_from_name(const Name *name);
int getsert_func_type(unsigned char *buf, size_t size, bool reg);
TagInfo *getsert_tag(const Name *name, int typeindex);

void write_wasm_header(FILE *ofp);

typedef struct FuncExtra {
  Vector *funcall_results;  // [0]=Expr*, [1]=VarInfo*
  DataStorage *code;
  Vector *reloc_code;  // <RelocInfo*>
  int setjmp_count;
  size_t offset;  // from code section top (exclude section size).
} FuncExtra;

Expr *get_sp_var(void);
unsigned char to_wtype(const Type *type);
bool is_global_datsec_var(const VarInfo *varinfo, Scope *scope);  // Whether this global variable put on data section.

extern DataStorage *curcodeds;
