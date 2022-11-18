// Emit code

#pragma once

#include <stdint.h>  // int64_t

typedef struct Vector Vector;

void emit_code(Vector *decls);

char *im(int64_t x);  // #x
char *immediate_offset(const char *reg, int offset);
char *pre_index(const char *reg, int offset);
char *post_index(const char *reg, int offset);
char *reg_offset(const char *base, const char *reg, const char *shift);
char *label_at_page(char *label, int flag);  // bit0=pageoff, bit1=got
