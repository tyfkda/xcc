#include "unistd.h"

#if defined(__linux__)
#include "_syscall.h"

ssize_t write(int fd, const void *str, size_t len) {
  ssize_t ret;
  SYSCALL_RET(__NR_write, ret, "r"(fd), "r"(str), "r"(len));
  return ret;
}

#elif defined(__APPLE__)

// Use libc.
#define USE_LIBC

#else
#error Target not supported
#endif
