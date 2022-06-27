#if !defined(__WASM)
#include "unistd.h"

int chmod(const char *pathname, /*mode_t*/int mode) {
  __asm("mov $90, %eax\n"  // __NR_chmod
        "syscall");
}
#endif
