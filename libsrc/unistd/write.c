#include "unistd.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defined(__XV6)
ssize_t write(int fd, const void *str, size_t len) {
  __asm("mov $16, %eax\n"  // SYS_write
        "int $64");
}

#elif defined(__WASM)

#elif defined(__linux__)
#include "_syscall.h"

ssize_t write(int fd, const void *str, size_t len) {
  ssize_t ret;
  SYSCALL_RET(__NR_write, ret);
  return ret;
}

#elif defined(__APPLE__)

// Use libc.
#define USE_LIBC

#else
#error Target not supported
#endif
