#include "unistd.h"
#include "_syscall.h"

#if defined(__NR_unlink)
int unlink(const char *pathname) {
  int ret;
  SYSCALL_RET(__NR_unlink, ret);
  SET_ERRNO(ret);
  return ret;
}

#elif defined(__NR_unlinkat)
#include "fcntl.h"  // AT_FDCWD
int unlink(const char *pathname) {
  return unlinkat(AT_FDCWD, pathname, 0);
}
#endif
