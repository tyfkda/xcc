// ./xcc -c examples/const.c examples/extern.c
// ./ld const.o extern.o

#include "../libsrc/unistd/_syscall.h"

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

extern int value;

int main(void) {
  return value;
}
