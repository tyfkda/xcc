// Emit code

#pragma once

#include <stdint.h>  // int64_t

typedef struct Vector Vector;

void emit_code(Vector *decls);

char *im(int64_t x);
