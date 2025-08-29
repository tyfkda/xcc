#include "unistd.h"
#include "_syscall.h"

#if defined(__NR_ioctl)
int ioctl(int fd, int request, ...) {
  int ret;
  SYSCALL_RET(__NR_ioctl, ret, "r"(fd), "r"(request));
  SET_ERRNO(ret);
  return ret;
}
#endif
