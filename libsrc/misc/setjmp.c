#if !defined(__WASM)
#include "setjmp.h"

int setjmp(jmp_buf env) {
  __asm("mov (%rsp), %rax\n"  // return address.
        "mov %rax, 0(%rdi)\n"
        "mov %rbp, 8(%rdi)\n"
        "mov %rsp, 16(%rdi)\n"
        "mov %rbx, 24(%rdi)\n"
        "mov %r12, 32(%rdi)\n"
        "mov %r13, 40(%rdi)\n"
        "mov %r14, 48(%rdi)\n"
        "mov %r15, 56(%rdi)\n"
        "xor %eax, %eax");
}
#endif
