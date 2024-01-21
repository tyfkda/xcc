#pragma once

#include <stdbool.h>

typedef struct File File;
typedef struct Vector Vector;

typedef struct {
  Vector *files;  // <File*>
} WasmLinker;

void linker_init(WasmLinker *linker);
bool read_wasm_obj(WasmLinker *linker, const char *filename);
