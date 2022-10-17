#if !defined(__WASM)
#include "fcntl.h"
#include "unistd.h"
#include "errno.h"
#include "_syscall.h"
#include "assert.h"

int open(const char *fn, int flag, ...) {
#if defined(__x86_64__)
  int ret;
  SYSCALL_RET(__NR_open, ret);
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;

#elif defined(__aarch64__)
#define STR(x)   STR2(x)
#define STR2(x)  #x

  int ret;
  __asm("mov w3, w2\n"
	"mov w2, w1\n"
	"mov x1, x0\n"
	"mov w0, #" STR(AT_FDCWD) "\n");
  SYSCALL_RET(__NR_openat, ret);
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;

#else
#error unknown target
#endif
}
#endif
