#if !defined(__WASM)
#include "unistd.h"

int dup(int fd) {
  __asm("mov $32, %eax\n"  // __NR_dup
        "syscall");
}
#endif
