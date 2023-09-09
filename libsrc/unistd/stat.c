#if !defined(__WASM) && !defined(__APPLE__)
#include "sys/stat.h"
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defined(__NR_stat)
int stat(const char *pathname, struct stat *buf) {
  int ret;
  SYSCALL_RET(__NR_stat, ret);
  return ret;
}
#elif defined(__NR_newfstatat)
#include "fcntl.h"  // AT_FDCWD
int stat(const char *pathname, struct stat *buf) {
  return fstatat(AT_FDCWD, pathname, buf, 0);
}
#endif
#endif
