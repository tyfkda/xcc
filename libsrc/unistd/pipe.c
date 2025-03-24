#include "unistd.h"
#include "_syscall.h"

#if defined(__NR_pipe)
int pipe(int *pipefd) {
  int ret;
  SYSCALL_RET(__NR_pipe, ret);
  SET_ERRNO(ret);
  return ret;
}
#elif defined(__NR_pipe2)
int pipe(int *pipefd) {
  return pipe2(pipefd, 0);
}
#endif
