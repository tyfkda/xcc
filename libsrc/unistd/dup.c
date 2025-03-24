#include "unistd.h"
#include "_syscall.h"

int dup(int fd) {
  int ret;
  SYSCALL_RET(__NR_dup, ret);
  SET_ERRNO(ret);
  return ret;
}
