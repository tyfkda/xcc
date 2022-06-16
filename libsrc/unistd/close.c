#include "unistd.h"

int close(int fd) {
  __asm("mov $3, %eax\n"  // __NR_close
        "syscall\n");
}
