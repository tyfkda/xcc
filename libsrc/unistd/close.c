#include "unistd.h"
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

int close(int fd) {
  int ret;
  SYSCALL_RET(__NR_close, ret);
  return ret;
}
