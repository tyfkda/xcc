#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"
#include "assert.h"

int ioctl(int fd, int request, ...) {
#if defined(__aarch64__)
  assert(!"TODO");
#else
  int ret;
  SYSCALL_RET(__NR_ioctl, ret);
  return ret;
#endif
}
#endif
