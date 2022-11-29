#if !defined(__WASM) && !defined(__APPLE__)
#include "unistd.h"
#include "_syscall.h"
#include "fcntl.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defined(__NR_unlinkat)
int unlinkat(int dirfd, const char *pathname, int flags) {
  int ret;
  SYSCALL_RET(__NR_unlinkat, ret);
  return ret;
}
#endif

#if defined(__NR_unlink)
int unlink(const char *pathname) {
  int ret;
  SYSCALL_RET(__NR_unlink, ret);
  return ret;
}
#elif defined(__NR_unlinkat)
int unlink(const char *pathname) {
  return unlinkat(AT_FDCWD, pathname, 0);
}
#endif
#endif
