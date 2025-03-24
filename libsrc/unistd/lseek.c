#include "unistd.h"
#include "_syscall.h"

off_t lseek(int fd, off_t offset, int whence) {
  off_t ret;
  SYSCALL_RET(__NR_lseek, ret);
  SET_ERRNO(ret);
  return ret;
}
