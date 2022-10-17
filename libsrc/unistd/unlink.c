#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"

int unlink(const char *pathname) {
  int ret;
  SYSCALL_RET(__NR_unlink, ret);
  return ret;
}
#endif
