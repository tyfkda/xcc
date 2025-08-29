#include "sys/stat.h"
#include "_syscall.h"

#if defined(__NR_fstatat)
int fstatat(int fd, const char *pathname, struct stat *buf, int flag) {
  int ret;
#if defined(__x86_64__)
  SYSCALL_ARGCOUNT(4);
#endif
  SYSCALL_RET(__NR_fstatat, ret, "r"(fd), "r"(pathname), "r"(buf), "r"(flag));
  SET_ERRNO(ret);
  return ret;
}

#elif defined(__NR_newfstatat)
int fstatat(int fd, const char *pathname, struct stat *buf, int flag) {
  int ret;
#if defined(__x86_64__)
  SYSCALL_ARGCOUNT(4);
#endif
  SYSCALL_RET(__NR_newfstatat, ret, "r"(fd), "r"(pathname), "r"(buf), "r"(flag));
  SET_ERRNO(ret);
  return ret;
}
#endif
