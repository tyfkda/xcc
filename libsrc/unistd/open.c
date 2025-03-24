// #include "fcntl.h"  // Avoid conflicting with prototype definition.
#include "sys/stat.h"  // mode_t
#include "_syscall.h"

#if defined(__NR_open)
int open(const char *fn, int flag, mode_t mode) {
  int ret;
  SYSCALL_RET(__NR_open, ret);
  SET_ERRNO(ret);
  return ret;
}
#elif defined(__NR_openat)

// #include "fcntl.h"  // AT_FDCWD
#define AT_FDCWD  -100  // Avoid conflicting with prototype definition.

int open(const char *fn, int flag, mode_t mode) {
  extern int openat(int dirfd, const char *fn, int flag, mode_t mode);
  return openat(AT_FDCWD, fn, flag, mode);
}
#endif
