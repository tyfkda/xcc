#if !defined(__WASM) && !defined(__APPLE__)
#include "unistd.h"
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

int dup(int fd) {
  int ret;
  SYSCALL_RET(__NR_dup, ret);
  return ret;
}
#endif
