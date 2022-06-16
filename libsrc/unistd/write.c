#include "unistd.h"

#if defined(__XV6)
ssize_t write(int fd, const void *str, size_t len) {
  __asm("mov $16, %eax\n"  // SYS_write
        "int $64");
}

#elif defined(__WASM)

#elif defined(__linux__)
ssize_t write(int fd, const void *str, size_t len) {
  ssize_t ret;
#if defined(__XCC)
  __asm("mov $1, %eax\n"  // __NR_write
        "syscall"
        : "=r"(ret));
#else
  __asm("mov $1, %%eax\n"  // __NR_write
        "syscall"
        : "=r"(ret));
#endif
  return ret;
}

#elif defined(__APPLE__)

// Use libc.
#define USE_LIBC

#else
#error Target not supported
#endif
