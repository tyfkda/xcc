#include "unistd.h"

int pipe(int *pipefd) {
  __asm("mov $22, %eax\n"  // __NR_pipe
        "syscall");
}
