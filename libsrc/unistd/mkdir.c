#include "sys/stat.h"
#include "_syscall.h"
#include "errno.h"

#if defined(__NR_mkdir)
int mkdir(const char *pathname, mode_t mode) {
  int ret;
  SYSCALL_RET(__NR_mkdir, ret);
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;
}

#elif defined(__NR_mkdirat)
#include "fcntl.h"

int mkdir(const char *pathname, mode_t mode) {
  return mkdirat(AT_FDCWD, pathname, mode);
}
#endif
