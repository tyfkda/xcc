#if !defined(__WASM)
#include "sys/stat.h"  // mode_t
#include "_syscall.h"

#if defined(__NR_chmod)
int chmod(const char *pathname, mode_t mode) {
  int ret;
  SYSCALL_RET(__NR_chmod, ret);
  return ret;
}
#else

#include "fcntl.h"  // AT_FDCWD

int chmod(const char *pathname, mode_t mode) {
  return fchmodat(AT_FDCWD, pathname, mode, 0);
}
#endif
#endif
