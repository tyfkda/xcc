#include "signal.h"
#include "_syscall.h"

long syscall(long number, ...) {
  (void)number;
  int ret;
#if defined(__x86_64__)
  __asm volatile(
      "mov %%rdi, %%rax\n"
      "mov %%rsi, %%rdi\n"
      "mov %%rdx, %%rsi\n"
      "mov %%rcx, %%rdx\n"
      "mov %%r8, %%r10\n"
      "mov %%r9, %%r8\n"
      // TODO: 6th paramter.
      "syscall\n"
      "mov %%eax, %0"
      : "=r"(ret));

#elif defined(__aarch64__)

#endif
  SET_ERRNO(ret);
  return -1;
}
