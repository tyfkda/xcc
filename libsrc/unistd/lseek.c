#if !defined(__WASM) && !defined(__APPLE__)
#include "unistd.h"
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

off_t lseek(int fd, off_t offset, int whence) {
  off_t ret;
  SYSCALL_RET(__NR_lseek, ret);
  return ret;
}
#endif
