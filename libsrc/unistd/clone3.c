#if !defined(__WASM) && !defined(__APPLE__)
#include "unistd.h"
#include "_syscall.h"
#include "signal.h"

#if defined(__NR_clone3)
#include "stdint.h"

long clone3(struct clone_args *cl_args, size_t size) {
  long ret;
  SYSCALL_RET(__NR_clone3, ret);
  return ret;
}
#endif
#endif
