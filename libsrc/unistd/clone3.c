#include "unistd.h"
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defined(__NR_clone3)
#include "stdint.h"

long clone3(struct clone_args *cl_args, size_t size) {
  long ret;
  SYSCALL_RET(__NR_clone3, ret);
  return ret;
}
#endif
