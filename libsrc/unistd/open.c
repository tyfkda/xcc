#if !defined(__WASM)
#include "unistd.h"
#include "errno.h"
#include "_syscall.h"

int open(const char *fn, int flag, ...) {
  int ret;
  SYSCALL_RET(__NR_open, ret);
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;
}
#endif
