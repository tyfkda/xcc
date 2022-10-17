#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"
#include "assert.h"

int pipe(int *pipefd) {
#if defined(__aarch64__)
  assert(!"TODO");
#else
  int ret;
  SYSCALL_RET(__NR_pipe, ret);
  return ret;
#endif
}
#endif
