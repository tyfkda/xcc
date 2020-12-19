#pragma once

#include <stddef.h>  // size_t
#include <stdint.h>

typedef struct DataStorage DataStorage;
typedef struct Vector Vector;

extern const char RETVAL_NAME[];

extern DataStorage *code;
extern Vector *functions;

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
