// Emit code

#pragma once

#include <stdint.h>  // int64_t

typedef struct Vector Vector;

char *im(int64_t x);
char *immediate_offset(int offset, const char *reg);
