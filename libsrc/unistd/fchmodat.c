#if !defined(__WASM)
#include "sys/stat.h"  // mode_t
#include "_syscall.h"

#if defined(__NR_fchmodat)
int fchmodat(int dirfd, const char *pathname, mode_t mode, int flags) {
  int ret;
  SYSCALL_RET(__NR_fchmodat, ret);
  return ret;
}
#endif
#endif
