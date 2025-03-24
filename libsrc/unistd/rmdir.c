#include "sys/stat.h"
#include "_syscall.h"

#if defined(__NR_rmdir)
int rmdir(const char *pathname) {
  int ret;
  SYSCALL_RET(__NR_rmdir, ret);
  SET_ERRNO(ret);
  return ret;
}

#elif defined(__NR_unlinkat)
#include "unistd.h"
#include "fcntl.h"

int rmdir(const char *pathname) {
  return unlinkat(AT_FDCWD, pathname, AT_REMOVEDIR);
}
#endif
