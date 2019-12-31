#pragma once

#include <stdint.h>  // uintptr_t

typedef uintptr_t jmp_buf[8];

int setjmp(jmp_buf env);
void longjmp(jmp_buf env, int result);
