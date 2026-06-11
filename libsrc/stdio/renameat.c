#include "stdio.h"

#if !defined(__wasm)
#include "unistd.h"
#include "../unistd/_syscall.h"

#if defined(__NR_renameat)
int renameat(int olddirfd, const char *oldpath, int newdirfd, const char *newpath) {
  ssize_t ret;
  SYSCALL_RET(__NR_renameat, ret, "r"(olddirfd), "r"(oldpath), "r"(newdirfd), "r"(newpath));
  SET_ERRNO(ret);
  return ret;
}
#endif
#endif
