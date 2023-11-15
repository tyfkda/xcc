#include "unistd.h"
#include "_syscall.h"

#if defined(__NR_unlinkat)
#include "errno.h"

int unlinkat(int dirfd, const char *pathname, int flags) {
  int ret;
  SYSCALL_RET(__NR_unlinkat, ret);
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;
}
#endif
