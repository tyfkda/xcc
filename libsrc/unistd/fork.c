#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"

pid_t fork(void) {
  pid_t ret;
  SYSCALL_RET(__NR_fork, ret);
  return ret;
}
#endif
