#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"
#include "assert.h"
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

int pipe(int *pipefd) {
#if defined(__NR_pipe)
  int ret;
  SYSCALL_RET(__NR_pipe, ret);
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;
#elif defined(__NR_pipe2)
  return pipe2(pipefd, 0);
#else
  assert(!"TODO");
#endif
}
#endif
