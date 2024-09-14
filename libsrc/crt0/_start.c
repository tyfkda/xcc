#include "stdlib.h"  // atexit, exit
#include "../stdlib/_exit.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-function"
#endif

#if defined(__linux__)
extern void (*__init_array_start []) (void);  // __attribute__((weak));
extern void (*__init_array_end []) (void);  // __attribute__((weak));
extern void (*__fini_array_start []) (void);  // __attribute__((weak));
extern void (*__fini_array_end []) (void);  // __attribute__((weak));

#if defined(__riscv)
int __dummy = 123;  // Force .data section to be exist to avoid ELF-load error on spike/pk.
#endif

static void call_fini_funcs(void) {
  for (void (**pp)(void) = __fini_array_start; pp < __fini_array_end; ++pp)
    (*pp)();
}

static void start2(int argc, char *argv[], char *env[]) {
  extern int main(int, char**, char **);
#ifndef __APPLE__
  extern char **environ;
  environ = env;
#endif

  static OnExitChain chain = {NULL, call_fini_funcs};
  chain.next = __on_exit_chain;
  __on_exit_chain = &chain;

  for (void (**pp)(void) = __init_array_start; pp < __init_array_end; ++pp)
    (*pp)();

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
  __asm("ldr x0, [sp]\n"
        "add x1, sp, #8\n"
        "add x2, x1, #8\n"
        "add x2, x2, x0, lsl #3\n"
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
