#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"
#include "fcntl.h"

#if defined(__NR_unlinkat)
int unlinkat(int dirfd, const char *pathname, int flags) {
  int ret;
  SYSCALL_RET(__NR_unlinkat, ret);
  return ret;
}
#endif
#endif
