// Emit code

#pragma once

typedef struct Vector Vector;

extern int stackpos;

#define PUSH_STACK_POS()  do { stackpos += WORD_SIZE; } while (0)
#define POP_STACK_POS()   do { stackpos -= WORD_SIZE; } while (0)

void emit_code(Vector *decls);
