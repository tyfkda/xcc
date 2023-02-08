#pragma once

#include <stdint.h>  // uintptr_t

#if defined(__WASM)
typedef uintptr_t jmp_buf[1];  // Stack pointer.

#elif defined(__aarch64__)
typedef uintptr_t jmp_buf[192 / 8];

#elif defined(__x86_64__)
typedef uintptr_t jmp_buf[200 / 8];  // GCC
#endif

int setjmp(jmp_buf env);
void longjmp(jmp_buf env, int result);

#ifdef __WASM
#define setjmp(env)           __builtin_setjmp(env)
#define longjmp(env, result)  __builtin_longjmp(env, result)
#endif
