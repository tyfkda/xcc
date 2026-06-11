#include "stdio.h"

#if !defined(__wasm)
#include "fcntl.h"
#include "unistd.h"
#include "../unistd/_syscall.h"

#if defined(__NR_rename)
int rename(const char *oldpath, const char *newpath) {
  ssize_t ret;
  SYSCALL_RET(__NR_rename, ret, "r"(oldpath), "r"(newpath));
  SET_ERRNO(ret);
  return ret;
}
#elif defined(__NR_renameat)

int rename(const char *oldpath, const char *newpath) {
  return renameat(AT_FDCWD, oldpath, AT_FDCWD, newpath);
}
#endif
#endif
