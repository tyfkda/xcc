#include "stdlib.h"  // atexit, exit

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-function"
#endif

char **environ;

#if defined(__linux__)

#include "stdio.h"  // fflush
#include "../stdio/_fileman.h"

extern FILEMAN __fileman;

inline void __flush_all_files(void) {
  fflush(stdout);
  fflush(stderr);

  struct FILE **files = __fileman.opened;
  for (int i = 0, length = __fileman.length; i < length; ++i)
    fflush(files[i]);
}

static void _atexit_proc(void) {
  __flush_all_files();
}

static void start2(int argc, char *argv[], char *env[]) {
  extern int main(int, char**, char **);
  environ = env;
  atexit(_atexit_proc);
  int ec = main(argc, argv, env);
  exit(ec);
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
#elif defined(__riscv)
  __asm("lw a0, 0(sp)\n"  // argc
        "addi a1, sp, 8\n"  // argv
        "slli a2, a0, 3\n"
        "addi a2, a2, 8\n"
        "add  a2, a2, a1\n"  // envp
        "j start2\n");
#else
#error unknown target
#endif
}

#elif defined(__APPLE__)

// Use libc.

extern void exit(int code);

#else
#error Target not supported
#endif
