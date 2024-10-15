// #include "fcntl.h"  // Avoid conflicting with prototype definition.
// #include "unistd.h"
#include "errno.h"
#include "sys/stat.h"  // mode_t
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defined(__NR_openat)
int openat(int dirfd, const char *fn, int flag, mode_t mode) {
  int ret;
#if defined(__x86_64__)
  SYSCALL_ARGCOUNT(4);
#endif
  SYSCALL_RET(__NR_openat, ret);
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;
}
#endif
