#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"

int ioctl(int fd, int request, ...) {
  int ret;
  SYSCALL_RET(__NR_ioctl, ret);
  return ret;
}
#endif
