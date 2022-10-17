#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"

ssize_t read(int fd, void *buf, size_t size) {
  ssize_t ret;
  SYSCALL_RET(__NR_read, ret);
  return ret;
}
#endif
