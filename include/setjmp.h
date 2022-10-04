#pragma once

#ifdef __WASM
#error cannot use setjmp on wasm
#endif

#include <stdint.h>  // uintptr_t

#ifdef __aarch64__
typedef uintptr_t jmp_buf[192 / 8];

#else
typedef uintptr_t jmp_buf[200 / 8];  // GCC
#endif

int setjmp(jmp_buf env);
void longjmp(jmp_buf env, int result);
