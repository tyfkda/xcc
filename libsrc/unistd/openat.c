#if !defined(__WASM)
// #include "fcntl.h"  // Avoid conflicting with prototype definition.
// #include "unistd.h"
#include "errno.h"
#include "sys/stat.h"  // mode_t
#include "_syscall.h"

#define AT_FDCWD  -100

#if defined(__NR_openat)
int openat(int dirfd, const char *fn, int flag, mode_t mode) {
  int ret;
  SYSCALL_RET(__NR_openat, ret);
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;
}
#endif

#endif
