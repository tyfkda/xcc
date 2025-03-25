#include "sys/stat.h"
#include "_syscall.h"

#if defined(__NR_lstat)
int lstat(const char *pathname, struct stat *buf) {
  int ret;
  SYSCALL_RET(__NR_lstat, ret, "r"(pathname), "r"(buf));
  SET_ERRNO(ret);
  return ret;
}
#elif defined(__NR_fstatat) || defined(__NR_newfstatat)
#include "fcntl.h"  // AT_FDCWD
int lstat(const char *pathname, struct stat *buf) {
  return fstatat(AT_FDCWD, pathname, buf, AT_SYMLINK_NOFOLLOW);
}
#endif
