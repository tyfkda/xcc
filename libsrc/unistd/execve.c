#include "unistd.h"

int execve(const char *path, char *const args[], char *const envp[]) {
  __asm("mov $59, %eax\n"  // __NR_execve
        "syscall");
}
