#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"
#include "fcntl.h"
#include "assert.h"

#if defined(__aarch64__)
int fchmodat(int dirfd, const char *pathname, /*mode_t*/int mode, int flags) {
  int ret;
  SYSCALL_RET(__NR_fchmodat, ret);
  return ret;
}
#endif

int chmod(const char *pathname, /*mode_t*/int mode) {
#if defined(__NR_chmod)
  int ret;
  SYSCALL_RET(__NR_chmod, ret);
  return ret;
#elif defined(__NR_fchmodat)
  return fchmodat(AT_FDCWD, pathname, mode, 0);
#else
  assert(!"TODO");
#endif
}
#endif
