#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"
#include "errno.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defined(__NR_pipe)
int pipe(int *pipefd) {
  int ret;
  SYSCALL_RET(__NR_pipe, ret);
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;
}
#elif defined(__NR_pipe2)
int pipe(int *pipefd) {
  return pipe2(pipefd, 0);
}
#endif
#endif
