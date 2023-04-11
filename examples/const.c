// ./xcc -c examples/const.c examples/extern.c
// ./ld const.o extern.o
// ./ld examples/const.o examples/extern.o

#include "../libsrc/unistd/_syscall.h"
#include "sys/types.h"  // ssize_t
#include "stdint.h"

static void proc_exit(int code) {
#ifdef __NR_exit_group
  SYSCALL(__NR_exit_group);
#endif
  SYSCALL(__NR_exit);
}

static void start2(int argc, char *argv[]) {
  extern int main(int, char**);
  int ec = main(argc, argv);
  proc_exit(ec);
}

void _start(void) {
#if defined(__x86_64__)
  __asm("mov (%rsp), %rdi\n"
        "lea 8(%rsp), %rsi\n"
        "lea 8(%rsi, %rdi, 8), %rdx\n"
        "jmp start2");
#elif defined(__aarch64__)
  __asm("mov x0, x1\n"
        "mov x1, x2\n"
        "mov x2, x3\n"
        "b start2");
#else
#error unknown target
#endif
}

ssize_t write(int fd, const void *str, size_t len) {
  ssize_t ret;
  SYSCALL_RET(__NR_write, ret);
  return ret;
}

extern int value;
extern const char message[];
extern int bss;

int main(void) {
  write(1, message, value);
  return bss;
}
