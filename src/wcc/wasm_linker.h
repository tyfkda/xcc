#pragma once

#include <stdbool.h>
#include <stdint.h>

#include <table.h>

typedef struct File File;
typedef struct Vector Vector;

#define WASI_MODULE_NAME  "wasi_snapshot_preview1"

typedef struct {
  Vector *files;  // <File*>
  Table defined, unresolved;
  Table indirect_functions;
  uint32_t unresolved_func_count;
  uint32_t data_end_address;

  const Name *sp_name;
  const Name *curbrk_name;
} WasmLinker;

void linker_init(WasmLinker *linker);
bool read_wasm_obj(WasmLinker *linker, const char *filename);
bool link_wasm_objs(WasmLinker *linker, Vector *exports);
