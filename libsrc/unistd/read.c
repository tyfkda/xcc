#if !defined(__WASM) && !defined(__APPLE__)
#include "unistd.h"
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

ssize_t read(int fd, void *buf, size_t size) {
  ssize_t ret;
  SYSCALL_RET(__NR_read, ret);
  return ret;
}
#endif
