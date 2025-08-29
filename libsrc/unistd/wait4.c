#if !defined(__APPLE__)
#include "sys/wait.h"
#include "_syscall.h"
#include "errno.h"

pid_t wait4(pid_t pid, int* status, int options, struct rusage *usage) {
  int ret;
#if defined(__x86_64__)
  SYSCALL_ARGCOUNT(4);
#endif
  SYSCALL_RET(__NR_wait4, ret, "r"(pid), "r"(status), "r"(options), "r"(usage));
  SET_ERRNO(ret);
  return ret;
}
#endif
