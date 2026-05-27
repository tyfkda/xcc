#include "unistd.h"
#include "_syscall.h"

pid_t getpid(void) {
  int ret;
  SYSCALL_RET(__NR_getpid, ret);
  SET_ERRNO(ret);
  return ret;
}
