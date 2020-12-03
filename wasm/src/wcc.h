#pragma once

#include <stddef.h>  // size_t
#include <stdint.h>

typedef struct Vector Vector;

extern unsigned char* code;
extern size_t codesize;

// gen_wasm
void gen(Vector *decls);
unsigned char *emit_leb128(unsigned char *buf, int32_t val);
unsigned char *emit_uleb128(unsigned char *buf, uint32_t val);
