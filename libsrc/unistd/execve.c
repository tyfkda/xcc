#include "unistd.h"
#include "_syscall.h"

int execve(const char *path, char *const args[], char *const envp[]) {
  int ret;
  SYSCALL_RET(__NR_execve, ret, "r"(path), "r"(args), "r"(envp));
  SET_ERRNO(ret);
  return ret;
}
