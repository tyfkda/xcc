#include "stdlib.h"

#if defined(__XV6)
void exit(int code) {
  __asm("mov $2, %eax\n"  // SYS_exit
        "int $64");
}

#elif defined(__WASM)

#elif defined(__linux__)
#include "../unistd/_syscall.h"

void exit(int code) {
  SYSCALL(__NR_exit);
}

#elif defined(__APPLE__)

// Use libc.

extern void exit(int code);

#else
#error Target not supported
#endif
