#include "sys/stat.h"
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defined(__NR_newfstatat)
int fstatat(int fd, const char *pathname, struct stat *buf, int flag) {
  int ret;
  SYSCALL_RET(__NR_newfstatat, ret);
  return ret;
}
#endif
