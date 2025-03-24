#include "unistd.h"
#include "_syscall.h"

int kill(pid_t pid, int sig) {
  int ret;
  SYSCALL_RET(__NR_kill, ret);
  SET_ERRNO(ret);
  return ret;
}
