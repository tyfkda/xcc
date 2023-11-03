#pragma once

#if defined(__x86_64__)

#define SYSCALL(no)  _SYSCALL2(no)
#define _SYSCALL2(no) \
    __asm("mov $" #no ", %eax\n" \
          "syscall")

#define SYSCALL_RET(no, ret)  _SYSCALL_RET2(no, ret)
#define _SYSCALL_RET2(no, ret) \
    __asm("mov $" #no ", %%eax\n" \
          "syscall" \
          : "=r"(ret))

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
#define __NR_unlink  87
#define __NR_chmod   90
#define __NR_time    201
#define __NR_clock_gettime  228
#define __NR_newfstatat  262

#elif defined(__aarch64__)

#define SYSCALL(no)  _SYSCALL2(no)
#define _SYSCALL2(no) \
    __asm("mov x8, #" #no "\n" \
          "svc #0")

#define SYSCALL_RET(no, ret)  _SYSCALL_RET2(no, ret)
#define _SYSCALL_RET2(no, ret) \
    __asm("mov x8, #" #no "\n" \
          "svc #0" \
          : "=r"(ret))

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
#define __NR_newfstatat  79

#else
#error unknown
#endif
