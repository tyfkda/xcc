#include "unistd.h"
#include "_syscall.h"

#if defined(__NR_pipe2)
int pipe2(int *pipefd, int flag) {
  int ret;
  SYSCALL_RET(__NR_pipe2, ret);
  SET_ERRNO(ret);
  return ret;
}
#endif
