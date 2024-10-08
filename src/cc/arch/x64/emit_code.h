// Emit code

#pragma once

#include <stdint.h>  // int64_t

typedef struct Vector Vector;

char *im(int64_t x);  // $x
char *indirect(const char *base, const char *index, int scale);
char *offset_indirect(int offset, const char *base, const char *index, int scale);
char *label_indirect(const char *label, int64_t offset, const char *reg);
char *gotpcrel(char *label);
