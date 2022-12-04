#if !defined(__WASM) && !defined(__APPLE__)
#include "unistd.h"
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

int execve(const char *path, char *const args[], char *const envp[]) {
  int ret;
  SYSCALL_RET(__NR_execve, ret);
  return ret;
}
#endif
