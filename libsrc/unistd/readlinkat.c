#include "unistd.h"
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defined(__NR_readlinkat)
ssize_t readlinkat(int dirfd, const char *pathname, char *buf, size_t bufsiz) {
  ssize_t ret;
#if defined(__x86_64__)
  SYSCALL_ARGCOUNT(4);
#endif
  SYSCALL_RET(__NR_readlinkat, ret, "r"(dirfd), "r"(pathname), "r"(buf), "r"(bufsiz));
  SET_ERRNO(ret);
  return ret;
}
#endif
