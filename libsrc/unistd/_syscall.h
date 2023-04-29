#pragma once

#include "errno.h"

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

#define __NR_read    0
#define __NR_write   1
#define __NR_open    2
#define __NR_close   3
#define __NR_stat    4
#define __NR_fstat   5
#define __NR_lstat   6
#define __NR_lseek   8
#define __NR_brk     12
#define __NR_ioctl   16
#define __NR_pipe    22
#define __NR_dup     32
#define __NR_fork    57
#define __NR_execve  59
#define __NR_exit    60
#define __NR_wait4   61
#define __NR_kill    62
#define __NR_getcwd  79
#define __NR_chdir   80
#define __NR_mkdir   83
#define __NR_rmdir   84
#define __NR_unlink  87
#define __NR_readlink  89
#define __NR_chmod   90
#define __NR_time    201
#define __NR_clock_gettime  228
#define __NR_mkdirat     258
#define __NR_newfstatat  262
#define __NR_readlinkat  267

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

#define __NR_read    63
#define __NR_write   64
//#define __NR_open    2
#define __NR_openat    56
#define __NR_close   57
#define __NR_fstat   80
#define __NR_lseek   62
#define __NR_brk     214
//#define __NR_ioctl   16
#define __NR_pipe2    59
#define __NR_dup     23
//#define __NR_clone    220
#define __NR_clone3    435
#define __NR_execve  221
#define __NR_exit    93
#define __NR_exit_group  94
#define __NR_wait4   260
#define __NR_kill    129
#define __NR_getcwd  17
#define __NR_chdir   49
#define __NR_unlinkat  35
#define __NR_fchmodat   53
#define __NR_clock_gettime  113
#define __NR_mkdirat     34
#define __NR_newfstatat  79

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

#define __NR_getcwd    17
#define __NR_dup       23
#define __NR_chdir     49
#define __NR_openat    56
#define __NR_close     57
#define __NR_lseek     62
#define __NR_read      63
#define __NR_write     64
#define __NR_exit      93
#define __NR_kill      129
#define __NR_brk       214
#define __NR_execve    221
#define __NR_wait4     260
#define __NR_fstat     80
#define __NR_fstatat   79
#define __NR_mkdirat   34
#define __NR_unlinkat  35
#define __NR_readlinkat  78
#define __NR_newfstatat  79

#else
#error unknown
#endif

#define SET_ERRNO(res)  do { if ((res) < 0) { errno = -(res); res = -1; } } while (0)

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif
