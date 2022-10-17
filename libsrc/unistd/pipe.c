#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"

int pipe(int *pipefd) {
  int ret;
  SYSCALL_RET(__NR_pipe, ret);
  return ret;
}
#endif
