#include "unistd.h"
#include "_syscall.h"

int chdir(const char *path) {
  int ret;
  SYSCALL_RET(__NR_chdir, ret, "r"(path));
  SET_ERRNO(ret);
  return ret;
}
