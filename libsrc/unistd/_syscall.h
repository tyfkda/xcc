#pragma once

#include "errno.h"
#include "sys/syscall.h"

#if defined(__x86_64__)

#define SYSCALL(no, ...) \
    __asm volatile( \
        "mov %0, %%eax\n" \
        "syscall" \
        : /* no output */ \
        : "r"(no), ## __VA_ARGS__)

#define SYSCALL_RETW(no, ret, ...) \
    __asm volatile( \
        "mov %1, %%eax\n" \
        "syscall\n" \
        "mov %%eax, %0" \
        : "=r"(ret) \
        : "r"(no), ## __VA_ARGS__)

#define SYSCALL_RETL(no, ret, ...) \
    __asm volatile( \
        "mov %1, %%eax\n" \
        "syscall\n" \
        "mov %%rax, %0" \
        : "=r"(ret) \
        : "r"(no), ## __VA_ARGS__)

#define SYSCALL_RET(no, ret, ...) \
    do { \
      switch (sizeof(ret)) { \
      case 4: SYSCALL_RETW(no, ret, ## __VA_ARGS__); break; \
      case 8: SYSCALL_RETL(no, ret, ## __VA_ARGS__); break; \
      default: __asm("unexpected"); break; \
      }  \
    } while (0)

// 4th parameter for syscall is `%r10`. `%r10` is caller save so no need to save/restore
#define SYSCALL_ARGCOUNT(n)  do { if ((n) >= 4) __asm volatile("mov %rcx, %r10"); } while (0)

#elif defined(__aarch64__)

#define SYSCALL(no, ...) \
    __asm volatile( \
        "mov x8, %0\n" /* immediate range limited (12bit) */ \
        "svc #0" \
        : /* no output */ \
        : "r"(no), ## __VA_ARGS__)

#define SYSCALL_RET(no, ret, ...) \
    __asm volatile( \
        "mov x8, %1\n" /* immediate range limited (12bit) */ \
        "svc #0\n" \
        "mov %0, x0\n" \
        : "=r"(ret) \
        : "r"(no), ## __VA_ARGS__)

#define SYSCALL_RETW(no, ret, ...) \
    __asm volatile( \
        "mov x8, %1\n" /* immediate range limited (12bit) */ \
        "svc #0\n" \
        "mov %0, w0\n" \
        : "=r"(ret) \
        : "r"(no), ## __VA_ARGS__)

#define SYSCALL_RETL(no, ret, ...) \
    __asm volatile( \
        "mov x8, %1\n" /* immediate range limited (12bit) */ \
        "svc #0\n" \
        "mov %0, x0\n" \
        : "=r"(ret) \
        : "r"(no), ## __VA_ARGS__)

#define SYSCALL_RET(no, ret, ...) \
    do { \
      switch (sizeof(ret)) { \
      case 4: SYSCALL_RETW(no, ret, ## __VA_ARGS__); break; \
      case 8: SYSCALL_RETL(no, ret, ## __VA_ARGS__); break; \
      default: __asm("unexpected"); break; \
      }  \
    } while (0)

#elif defined(__riscv)

#define SYSCALL(no, ...) \
    __asm volatile( \
        "li a7, %0\n" /* immediate only */ \
        "ecall" \
        : /* no output*/ \
        : "ri"(no), ## __VA_ARGS__)

#define SYSCALL_RET(no, ret, ...) \
    __asm volatile( \
        "li a7, %1\n" /* immediate only */ \
        "ecall\n" \
        "mv %0, a0" \
        : "=r"(ret) \
        : "ri"(no), ## __VA_ARGS__)

#else
#error unknown
#endif

#define SET_ERRNO(res)  do { if ((res) < 0) { errno = -(res); res = -1; } } while (0)

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif
