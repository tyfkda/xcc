#pragma once

#define SYSCALL(no)  _SYSCALL2(no)
#define _SYSCALL2(no) \
    __asm("mov $" #no ", %eax\n" \
          "syscall")

#define SYSCALL_RET(no, ret)  _SYSCALL_RET2(no, ret)
#define _SYSCALL_RET2(no, ret) \
    __asm("mov $" #no ", %eax\n" \
          "syscall" \
          : "=r"(ret))

#define __NR_read    0
#define __NR_write   1
#define __NR_open    2
#define __NR_close   3
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
#define __NR_unlink  87
#define __NR_chmod   90
