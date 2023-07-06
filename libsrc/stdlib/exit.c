#include "stdlib.h"

#if defined(__XV6)
void exit(int code) {
  __asm("mov $2, %eax\n"  // SYS_exit
        "int $64");
}

#elif defined(__linux__) || defined(__WASM)
#include "stdbool.h"

#if defined(__WASM)
#include "../_wasm/wasi.h"
#else
#include "../unistd/_syscall.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif
static void proc_exit(int code) __attribute__((noreturn));
static void proc_exit(int code) {
#ifdef __NR_exit_group
  SYSCALL(__NR_exit_group);
#endif
  SYSCALL(__NR_exit);
  for (;;)
    ;
}
#endif

void exit(int code) {
  // TODO: Guard multiple calls

  extern void __atexit_call(void);
  __atexit_call();

  proc_exit(code);
}

#elif defined(__APPLE__)

// Use libc.

extern void exit(int code);

#else
#error Target not supported
#endif
