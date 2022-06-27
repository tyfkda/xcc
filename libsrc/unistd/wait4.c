#if !defined(__WASM)
#include "unistd.h"

pid_t wait4(pid_t pid, int* status, int options, struct rusage *usage) {
  __asm("mov %rcx, %r10\n"  // 4th parameter for syscall is `%r10`. `%r10` is caller save so no need to save/restore
        "mov $61, %eax\n"  // __NR_wait4
        "syscall");
}
#endif
