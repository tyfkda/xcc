#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"

int chmod(const char *pathname, /*mode_t*/int mode) {
  int ret;
  SYSCALL_RET(__NR_chmod, ret);
  return ret;
}
#endif
