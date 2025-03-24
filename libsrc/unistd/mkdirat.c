#include "sys/stat.h"
#include "_syscall.h"

#if defined(__NR_mkdirat)
int mkdirat(int dirfd, const char *pathname, mode_t mode) {
  int ret;
  SYSCALL_RET(__NR_mkdirat, ret);
  SET_ERRNO(ret);
  return ret;
}
#endif
