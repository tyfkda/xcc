#if !defined(__WASM)
#include "setjmp.h"

#if defined(__x86_64__)
void longjmp(jmp_buf env, int result) {
  __asm("mov 0(%rdi), %rax\n"  // return address.
        "mov 8(%rdi), %rbp\n"
        "mov 16(%rdi), %rsp\n"
        "mov 24(%rdi), %rbx\n"
        "mov 32(%rdi), %r12\n"
        "mov 40(%rdi), %r13\n"
        "mov 48(%rdi), %r14\n"
        "mov 56(%rdi), %r15\n"
        "movsd 64(%rdi), %xmm0\n"
        "movsd 72(%rdi), %xmm1\n"
        "movsd 80(%rdi), %xmm2\n"
        "movsd 88(%rdi), %xmm3\n"
        "movsd 96(%rdi), %xmm4\n"
        "movsd 104(%rdi), %xmm5\n"
        "movsd 112(%rdi), %xmm6\n"
        "movsd 120(%rdi), %xmm7\n"
        "mov %rax, (%rsp)\n"  // Store return address onto top of the stack.
        "mov %esi, %eax\n"  // Result value.
        "test %eax, %eax\n"
        "jne .longjmp_0\n"
        "mov $1, %eax\n"
        ".longjmp_0:");
}
#elif defined(__aarch64__)
void longjmp(jmp_buf env, int result) {
  __asm("ldp fp, lr, [x0]\n"
        "ldp x9, x19, [x0, 16]\n"
        "ldp x20, x21, [x0, 32]\n"
        "ldp x22, x23, [x0, 48]\n"
        "ldp x24, x25, [x0, 64]\n"
        "ldp x26, x27, [x0, 80]\n"
        "ldp x28, x29, [x0, 96]\n"
        "ldp d8, d9, [x0, 112]\n"
        "ldp d10, d11, [x0, 128]\n"
        "ldp d12, d13, [x0, 144]\n"
        "ldp d14, d15, [x0, 160]\n"
        "mov sp, x9\n"
        "mov w0, w1\n"  // Result value.
        "cmp w0, wzr\n"
        "b.ne .longjmp_0\n"
        "mov w0, #1\n"
        ".longjmp_0:");
}
#endif
#endif
