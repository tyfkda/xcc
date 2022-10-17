#if !defined(__WASM)
#include "unistd.h"
#include "_syscall.h"
#include "assert.h"

int unlink(const char *pathname) {
#if defined(__aarch64__)
  assert(!"TODO");
#else
  int ret;
  SYSCALL_RET(__NR_unlink, ret);
  return ret;
#endif
}
#endif
