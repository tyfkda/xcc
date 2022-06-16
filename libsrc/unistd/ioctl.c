#include "unistd.h"

int ioctl(int fd, int request, ...) {
  __asm("mov $16, %eax\n"  // __NR_ioctl
        "syscall");
}
