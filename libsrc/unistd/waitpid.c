#include "sys/wait.h"
#include "stddef.h"  // NULL

pid_t waitpid(pid_t pid, int *status, int options) {
  return wait4(pid, status, options, NULL);
}
