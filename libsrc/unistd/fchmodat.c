#include "sys/stat.h"  // mode_t
#include "_syscall.h"

#if defined(__NR_fchmodat)
int fchmodat(int dirfd, const char *pathname, mode_t mode, int flags) {
  int ret;
#if defined(__x86_64__)
  SYSCALL_ARGCOUNT(4);
#endif
  SYSCALL_RET(__NR_fchmodat, ret, "r"(dirfd), "r"(pathname), "r"(mode), "r"(flags));
  SET_ERRNO(ret);
  return ret;
}
#endif
