#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"

off_t lseek(int fd, off_t offset, int whence) {
  off_t ret;
  SYSCALL_RET(__NR_lseek, ret);
  return ret;
}
#endif
