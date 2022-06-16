#include "unistd.h"

int kill(pid_t pid, int sig) {
  __asm("mov $62, %eax\n"  // __NR_kill
        "syscall");
}
