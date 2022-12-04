#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defined(__NR_ioctl)
int ioctl(int fd, int request, ...) {
  int ret;
  SYSCALL_RET(__NR_ioctl, ret);
  return ret;
}
#endif
#endif
