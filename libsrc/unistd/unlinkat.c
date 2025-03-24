#include "unistd.h"
#include "_syscall.h"

#if defined(__NR_unlinkat)
int unlinkat(int dirfd, const char *pathname, int flags) {
  int ret;
  SYSCALL_RET(__NR_unlinkat, ret);
  SET_ERRNO(ret);
  return ret;
}
#endif
