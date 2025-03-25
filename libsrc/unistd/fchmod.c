#include "sys/stat.h"  // mode_t
#include "_syscall.h"

#if defined(__NR_fchmod)
int fchmod(int fd, mode_t mode) {
  int ret;
  SYSCALL_RET(__NR_fchmod, ret, "r"(fd), "r"(mode));
  SET_ERRNO(ret);
  return ret;
}
#endif
