#include "unistd.h"
#include "_syscall.h"

#if defined(__NR_fork)
pid_t fork(void) {
  pid_t ret;
  SYSCALL_RET(__NR_fork, ret);
  return ret;
}

#elif defined(__NR_clone3)
#include "signal.h"

pid_t fork(void) {
  struct clone_args cl_args = {
    .exit_signal = SIGCHLD,
  };
  return clone3(&cl_args, sizeof(cl_args));
}
#endif
