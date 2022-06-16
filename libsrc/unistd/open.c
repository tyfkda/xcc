#include "unistd.h"
#include "errno.h"

int open(const char *fn, int flag, ...) {
  int ret;
  __asm("mov $2, %eax\n"  // __NR_open
        "syscall"
        : "=r"(ret));
  if (ret < 0) {
    errno = -ret;
    ret = -1;
  }
  return ret;
}
