#include "unistd.h"
#include "_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

int chdir(const char *path) {
  int ret;
  SYSCALL_RET(__NR_chdir, ret);
  return ret;
}
