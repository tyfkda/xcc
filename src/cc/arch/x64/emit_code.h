// Emit code

#pragma once

#include <stdint.h>  // intptr_t

typedef struct Vector Vector;

extern int stackpos;

#define PUSH_STACK_POS()  do { stackpos += WORD_SIZE; } while (0)
#define POP_STACK_POS()   do { stackpos -= WORD_SIZE; } while (0)

void emit_code(Vector *decls);

char *im(intptr_t x);  // $x
char *indirect(const char *base, const char *index, int scale);
char *offset_indirect(int offset, const char *base, const char *index, int scale);
char *label_indirect(const char *label, const char *reg);
char *gotpcrel(char *label);
