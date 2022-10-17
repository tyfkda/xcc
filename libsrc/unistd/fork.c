#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"
#include "assert.h"

pid_t fork(void) {
#if defined(__aarch64__)
  assert(!"TODO");
#else
  pid_t ret;
  SYSCALL_RET(__NR_fork, ret);
  return ret;
#endif
}
#endif
