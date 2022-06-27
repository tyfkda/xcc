#if !defined(__WASM)
#include "unistd.h"

off_t lseek(int fd, off_t offset, int whence) {
  __asm("mov $8, %eax\n"  // __NR_lseek
        "syscall\n");
}
#endif
