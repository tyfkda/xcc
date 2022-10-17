#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"

int kill(pid_t pid, int sig) {
  int ret;
  SYSCALL_RET(__NR_kill, ret);
  return ret;
}
#endif
