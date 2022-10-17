#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"
#include "assert.h"

int chmod(const char *pathname, /*mode_t*/int mode) {
#if defined(__aarch64__)
  assert(!"TODO");
#else
  int ret;
  SYSCALL_RET(__NR_chmod, ret);
  return ret;
#endif
}
#endif
