#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "table.h"

typedef struct File File;
typedef struct Vector Vector;

#define WASI_MODULE_NAME  "wasi_snapshot_preview1"

typedef struct {
  Vector *files;  // <File*>
  Table defined, unresolved;  // <SymbolInfo*>
  Table indirect_functions;
  uint32_t unresolved_func_count;
  uint32_t address_bottom;

  const Name *sp_name;
  const Name *curbrk_name;

  FILE *ofp;
} WasmLinker;

void linker_init(WasmLinker *linker);
bool read_wasm_obj(WasmLinker *linker, const char *filename);
bool link_wasm_objs(WasmLinker *linker, Vector *exports, uint32_t stack_size);
bool linker_emit_wasm(WasmLinker *linker, const char *ofn, Vector *exports);
