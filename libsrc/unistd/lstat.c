#if !defined(__WASM) && !defined(__APPLE__)
#include "unistd.h"
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defind(__NR_lstat)
int lstat(const char *pathname, struct stat *buf) {
  int ret;
  SYSCALL_RET(__NR_lstat, ret);
  return ret;
}
#elif defined(__NR_newfstatat)
#include "fcntl.h"  // AT_FDCWD
extern int newfstatat(int fd, const char *pathname, struct stat *buf, int flag);

int stat(const char *pathname, struct stat *buf) {
  return newfstatat(AT_FDCWD, pathname, buf, 0);  // TODO: Check flag.
}
#endif
#endif
