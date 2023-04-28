#if !defined(__WASM) && !defined(__APPLE__)
#include "unistd.h"
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defind(__NR_fstat)
int fstat(int fd, struct stat *buf) {
  int ret;
  SYSCALL_RET(__NR_fstat, ret);
  return ret;
}
#endif
#endif
