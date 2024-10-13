#include "sys/stat.h"
#include "_syscall.h"
#include "errno.h"

#if defined(__NR_mkdirat)
int mkdirat(int dirfd, const char *pathname, mode_t mode) {
  int ret;
  SYSCALL_RET(__NR_mkdirat, ret);
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;
}
#endif
