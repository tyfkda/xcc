#include "sys/wait.h"
#include "stddef.h"  // NULL

pid_t wait(int *status) {
  return wait4(-1, status, 0, NULL);
}
