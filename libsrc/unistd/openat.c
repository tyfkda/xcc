// #include "fcntl.h"  // Avoid conflicting with prototype definition.
// #include "unistd.h"
#include "sys/stat.h"  // mode_t
#include "_syscall.h"

#if defined(__NR_openat)
int openat(int dirfd, const char *fn, int flag, mode_t mode) {
  int ret;
#if defined(__x86_64__)
  SYSCALL_ARGCOUNT(4);
#endif
  SYSCALL_RET(__NR_openat, ret);
  SET_ERRNO(ret);
  return ret;
}
#endif
