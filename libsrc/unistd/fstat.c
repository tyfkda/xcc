#include "sys/stat.h"
#include "_syscall.h"

#if defined(__NR_fstat)
int fstat(int fd, struct stat *buf) {
  int ret;
  SYSCALL_RET(__NR_fstat, ret, "r"(fd), "r"(buf));
  SET_ERRNO(ret);
  return ret;
}
#endif
