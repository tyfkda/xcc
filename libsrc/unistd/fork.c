#include "unistd.h"

pid_t fork(void) {
  __asm("mov $57, %eax\n"  // __NR_fork
        "syscall");
}
