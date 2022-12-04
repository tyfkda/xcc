#if !defined(__WASM) && !defined(__APPLE__)
#include "unistd.h"
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

int kill(pid_t pid, int sig) {
  int ret;
  SYSCALL_RET(__NR_kill, ret);
  return ret;
}
#endif
