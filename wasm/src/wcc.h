#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>

typedef struct DataStorage DataStorage;
typedef struct Function Function;
typedef struct Table Table;
typedef struct Type Type;
typedef struct Vector Vector;

extern const char RETVAL_NAME[];

extern DataStorage *code;
extern Table func_info_table;
extern bool verbose;

#define VERBOSES(str)  do { if (verbose) printf("%s", str); } while (0)
#define VERBOSE(fmt, ...)  do { if (verbose) printf(fmt, __VA_ARGS__); } while (0)

typedef struct {
  Function *func;
  const Type *type;
  uint32_t index;
  int flag;
  uint32_t type_index;
} FuncInfo;

#define FF_REFERED   (1 << 0)

// traverse
void traverse_ast(Vector *decls, Vector *exports);

// gen_wasm
void gen(Vector *decls);
void emit_leb128(DataStorage *data, size_t pos, int32_t val);
void emit_uleb128(DataStorage *data, size_t pos, uint32_t val);

// wcc_util
typedef struct DataStorage {
  unsigned char *buf;
  size_t capacity;
  size_t len;
} DataStorage;

void data_init(DataStorage *data);
void data_reserve(DataStorage *data, size_t capacity);
void data_insert(DataStorage *data, size_t pos, const unsigned char *buf, size_t size);
void data_append(DataStorage *data, const unsigned char *buf, size_t size);
void data_push(DataStorage *data, unsigned char c);
void data_concat(DataStorage *data, DataStorage *data2);
