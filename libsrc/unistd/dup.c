#include "unistd.h"
#include "_syscall.h"

int dup(int fd) {
  int ret;
  SYSCALL_RET(__NR_dup, ret, "r"(fd));
  SET_ERRNO(ret);
  return ret;
}
