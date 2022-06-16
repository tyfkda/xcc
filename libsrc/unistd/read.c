#include "unistd.h"

ssize_t read(int fd, void *buf, size_t size) {
  __asm("mov $0, %eax\n"  // __NR_read
        "syscall");
}
