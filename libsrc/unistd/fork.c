#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"
#include "assert.h"

#if defined(__NR_clone3)
#include "signal.h"
#include "stdint.h"
typedef uint64_t u64;

struct clone_args {
  u64 flags;        /* Flags bit mask */
  u64 pidfd;        /* Where to store PID file descriptor
                       (int *) */
  u64 child_tid;    /* Where to store child TID,
                       in child's memory (pid_t *) */
  u64 parent_tid;   /* Where to store child TID,
                       in parent's memory (pid_t *) */
  u64 exit_signal;  /* Signal to deliver to parent on
                       child termination */
  u64 stack;        /* Pointer to lowest byte of stack */
  u64 stack_size;   /* Size of stack */
  u64 tls;          /* Location of new TLS */
  u64 set_tid;      /* Pointer to a pid_t array
                       (since Linux 5.5) */
  u64 set_tid_size; /* Number of elements in set_tid
                       (since Linux 5.5) */
  u64 cgroup;       /* File descriptor for target cgroup
                       of child (since Linux 5.7) */
};

static long _clone3(struct clone_args *cl_args, size_t size) {
  long ret;
  SYSCALL_RET(__NR_clone3, ret);
  return ret;
}
#endif

pid_t fork(void) {
#if defined(__NR_fork)
  pid_t ret;
  SYSCALL_RET(__NR_fork, ret);
  return ret;
#elif defined(__NR_clone3)
  struct clone_args cl_args = {
    .exit_signal = SIGCHLD,
  };
  return _clone3(&cl_args, sizeof(cl_args));
#else
  assert(!"TODO");
#endif
}
#endif
