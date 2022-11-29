#if !defined(__WASM) && !defined(__APPLE__)
#include "sys/wait.h"
#include "_syscall.h"
#include "errno.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

pid_t wait4(pid_t pid, int* status, int options, struct rusage *usage) {
  int ret;
#if defined(__x86_64__)
  __asm("mov %rcx, %r10");  // 4th parameter for syscall is `%r10`. `%r10` is caller save so no need to save/restore
#endif
  SYSCALL_RET(__NR_wait4, ret);
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;
}
#endif
