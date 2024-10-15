#if !defined(__APPLE__)
#include "sys/wait.h"
#include "_syscall.h"
#include "errno.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

pid_t wait4(pid_t pid, int* status, int options, struct rusage *usage) {
  int ret;
#if defined(__x86_64__)
  SYSCALL_ARGCOUNT(4);
#endif
  SYSCALL_RET(__NR_wait4, ret);
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;
}
#endif
