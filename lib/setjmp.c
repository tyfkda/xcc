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

void longjmp(jmp_buf env, int result) {
  __asm("mov 0(%rdi), %rax\n"
        "mov 8(%rdi), %rbp\n"
        "mov 16(%rdi), %rsp\n"
        "mov 24(%rdi), %rbx\n"
        "mov 32(%rdi), %r12\n"
        "mov 40(%rdi), %r13\n"
        "mov 48(%rdi), %r14\n"
        "mov 56(%rdi), %r15\n"
        "mov %rax, (%rsp)\n"
        "mov %esi, %eax\n"  // Result value.
        "test %eax, %eax\n"
        "jne .longjmp_0\n"
        "mov $1, %eax\n"
        ".longjmp_0:");
}
#endif
