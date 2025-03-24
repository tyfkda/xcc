#include "unistd.h"
#include "_syscall.h"

int close(int fd) {
  int ret;
  SYSCALL_RET(__NR_close, ret);
  SET_ERRNO(ret);
  return ret;
}
