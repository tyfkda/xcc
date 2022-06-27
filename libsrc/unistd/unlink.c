#if !defined(__WASM)
#include "unistd.h"

int unlink(const char *pathname) {
  __asm("mov $87, %eax\n"  // __NR_unlink
        "syscall");
}
#endif
