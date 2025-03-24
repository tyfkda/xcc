#include "sys/stat.h"
#include "_syscall.h"

#if defined(__NR_stat)
int stat(const char *pathname, struct stat *buf) {
  int ret;
  SYSCALL_RET(__NR_stat, ret);
  SET_ERRNO(ret);
  return ret;
}
#elif defined(__NR_fstatat) || defined(__NR_newfstatat)
#include "fcntl.h"  // AT_FDCWD
int stat(const char *pathname, struct stat *buf) {
  return fstatat(AT_FDCWD, pathname, buf, 0);
}
#endif
