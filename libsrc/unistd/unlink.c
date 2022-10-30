#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"
#include "fcntl.h"
#include "assert.h"

#if defined(__NR_unlinkat)
int unlinkat(int dirfd, const char *pathname, int flags) {
  int ret;
  SYSCALL_RET(__NR_unlinkat, ret);
  return ret;
}
#endif

int unlink(const char *pathname) {
#if defined(__NR_unlink)
  int ret;
  SYSCALL_RET(__NR_unlink, ret);
  return ret;
#elif defined(__NR_unlinkat)
  return unlinkat(AT_FDCWD, pathname, 0);
#else
  assert(!"TODO");
#endif
}
#endif
