#include "setjmp.h"

int setjmp(jmp_buf env) {
  __asm("mov (%rsp), %rax");  // return address.
  __asm("mov %rax, 0(%rdi)");
  __asm("mov %rbp, 8(%rdi)");
  __asm("mov %rsp, 16(%rdi)");
  __asm("mov %rbx, 24(%rdi)");
  __asm("mov %r12, 32(%rdi)");
  __asm("mov %r13, 40(%rdi)");
  __asm("mov %r14, 48(%rdi)");
  __asm("mov %r15, 56(%rdi)");
  __asm("xor %eax, %eax");
}

void longjmp(jmp_buf env, int result) {
  __asm("mov 0(%rdi), %rax");
  __asm("mov 8(%rdi), %rbp");
  __asm("mov 16(%rdi), %rsp");
  __asm("mov 24(%rdi), %rbx");
  __asm("mov 32(%rdi), %r12");
  __asm("mov 40(%rdi), %r13");
  __asm("mov 48(%rdi), %r14");
  __asm("mov 56(%rdi), %r15");
  __asm("mov %rax, (%rsp)");
  __asm("mov %esi, %eax");  // Result value.
  __asm("test %eax, %eax");
  __asm("jne .longjmp_0");
  __asm("mov $1, %eax");
  __asm("\n.longjmp_0:");
}
