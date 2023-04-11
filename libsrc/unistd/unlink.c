#if !defined(__APPLE__) || defined(__WASM)
#include "unistd.h"
#include "errno.h"

#if defined(__WASM)
extern int _unlink(const char *pathname);
#else
#include "_syscall.h"
#endif

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defined(__NR_unlink) || defined(__WASM)
int unlink(const char *pathname) {
  int ret;
#if defined(__WASM)
  ret = _unlink(pathname);
#else
  SYSCALL_RET(__NR_unlink, ret);
#endif
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
