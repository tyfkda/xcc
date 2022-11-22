#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"
#include "errno.h"

#if defined(__NR_pipe2)
int pipe2(int *pipefd, int flag) {
  int ret;
  SYSCALL_RET(__NR_pipe2, ret);
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;
}
#endif
#endif
