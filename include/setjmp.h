#pragma once

#include <stdint.h>  // uintptr_t

#if defined(__WASM)
typedef uintptr_t jmp_buf[2];  // 0=Stack pointer, 1=longjmp-result.

#elif defined(__aarch64__)
typedef uintptr_t jmp_buf[192 / 8];

#elif defined(__x86_64__)
typedef uintptr_t jmp_buf[200 / 8];  // GCC

#elif defined(__riscv)
typedef uintptr_t jmp_buf[208 / 8];
#endif

int setjmp(jmp_buf env);
_Noreturn void longjmp(jmp_buf env, int result);

#define _setjmp   setjmp
#define _longjmp  longjmp

#ifdef __WASM
#define setjmp(env)           __builtin_setjmp(env)
#define longjmp(env, result)  __builtin_longjmp(env, result)
#endif
