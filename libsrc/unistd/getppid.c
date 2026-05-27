#include "unistd.h"
#include "_syscall.h"

pid_t getppid(void) {
  int ret;
  SYSCALL_RET(__NR_getppid, ret);
  SET_ERRNO(ret);
  return ret;
}
