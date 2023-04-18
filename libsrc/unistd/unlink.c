#if !defined(__APPLE__)
#include "unistd.h"
#include "errno.h"

#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defined(__NR_unlink)
int unlink(const char *pathname) {
  int ret;
  SYSCALL_RET(__NR_unlink, ret);
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;
}

#elif defined(__NR_unlinkat)
#include "fcntl.h"  // AT_FDCWD
int unlink(const char *pathname) {
  return unlinkat(AT_FDCWD, pathname, 0);
}
#endif
#endif
