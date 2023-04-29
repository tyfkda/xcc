#include "unistd.h"
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defined(__NR_readlink)
ssize_t readlink(const char *pathname, char *buf, size_t bufsiz) {
  ssize_t ret;
  SYSCALL_RET(__NR_readlink, ret, "r"(pathname), "r"(buf), "r"(bufsiz));
  SET_ERRNO(ret);
  return ret;
}

#elif defined(__NR_readlinkat)
#include "fcntl.h"  // AT_FDCWD

ssize_t readlink(const char *pathname, char *buf, size_t bufsiz) {
  return readlinkat(AT_FDCWD, pathname, buf, bufsiz);
}
#endif
