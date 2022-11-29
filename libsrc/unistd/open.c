#if !defined(__WASM)
// #include "fcntl.h"  // Avoid conflicting with prototype definition.
#include "errno.h"
#include "sys/stat.h"  // mode_t
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defined(__NR_open)
int open(const char *fn, int flag, mode_t mode) {
  int ret;
  SYSCALL_RET(__NR_open, ret);
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;
}
#elif defined(__NR_openat)

#define AT_FDCWD  -100

int open(const char *fn, int flag, mode_t mode) {
  extern int openat(int dirfd, const char *fn, int flag, mode_t mode);
  return openat(AT_FDCWD, fn, flag, mode);
}
#endif

#endif
