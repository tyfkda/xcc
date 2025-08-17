#if !defined(__GNUC__)
#include "setjmp.h"

#if defined(__wasm)
// handled by builtin

#elif defined(__x86_64__)
int setjmp(jmp_buf env) {
#ifdef __NO_FLONUM
#define SAVE_FREGS  // Empty
#else
#define SAVE_FREGS \
    "movsd %xmm0, 64(%rdi)\n" \
    "movsd %xmm1, 72(%rdi)\n" \
    "movsd %xmm2, 80(%rdi)\n" \
    "movsd %xmm3, 88(%rdi)\n" \
    "movsd %xmm4, 96(%rdi)\n" \
    "movsd %xmm5, 104(%rdi)\n" \
    "movsd %xmm6, 112(%rdi)\n" \
    "movsd %xmm7, 120(%rdi)\n"
#endif
  __asm volatile(
      "mov (%rsp), %rax\n"  // return address.
      "mov %rax, 0(%rdi)\n"
      "mov %rbp, 8(%rdi)\n"
      "mov %rsp, 16(%rdi)\n"
      "mov %rbx, 24(%rdi)\n"
      "mov %r12, 32(%rdi)\n"
      "mov %r13, 40(%rdi)\n"
      "mov %r14, 48(%rdi)\n"
      "mov %r15, 56(%rdi)\n"
      SAVE_FREGS
      "xor %eax, %eax");
}
#elif defined(__aarch64__)
int setjmp(jmp_buf env) {
#ifdef __NO_FLONUM
#define SAVE_FREGS  // Empty
#else
#define SAVE_FREGS \
    "stp d8, d9, [x0, #112]\n" \
    "stp d10, d11, [x0, #128]\n" \
    "stp d12, d13, [x0, #144]\n" \
    "stp d14, d15, [x0, #160]\n"
#endif
  __asm volatile(
      "stp fp, lr, [x0]\n"
      "mov x9, sp\n"
      "stp x9, x19, [x0, #16]\n"
      "stp x20, x21, [x0, #32]\n"
      "stp x22, x23, [x0, #48]\n"
      "stp x24, x25, [x0, #64]\n"
      "stp x26, x27, [x0, #80]\n"
      "str x28, [x0, #96]\n"  // x29 is already saved (fp).
      SAVE_FREGS
      "mov w0, wzr");
}
#elif defined(__riscv)
int setjmp(jmp_buf env) {
#ifdef __NO_FLONUM
#define SAVE_FREGS  // Empty
#else
#define SAVE_FREGS \
    "fsd fs0, 112(a0)\n" \
    "fsd fs1, 120(a0)\n" \
    "fsd fs2, 128(a0)\n" \
    "fsd fs3, 136(a0)\n" \
    "fsd fs4, 144(a0)\n" \
    "fsd fs5, 152(a0)\n" \
    "fsd fs6, 160(a0)\n" \
    "fsd fs7, 168(a0)\n" \
    "fsd fs8, 176(a0)\n" \
    "fsd fs9, 184(a0)\n" \
    "fsd fs10, 192(a0)\n" \
    "fsd fs11, 200(a0)\n"
#endif
  __asm volatile(
      "sd ra, 0(a0)\n"
      "sd sp, 8(a0)\n"
      "sd fp, 16(a0)\n"
      "sd s1, 24(a0)\n"
      "sd s2, 32(a0)\n"
      "sd s3, 40(a0)\n"
      "sd s4, 48(a0)\n"
      "sd s5, 56(a0)\n"
      "sd s6, 64(a0)\n"
      "sd s7, 72(a0)\n"
      "sd s8, 80(a0)\n"
      "sd s9, 88(a0)\n"
      "sd s10, 96(a0)\n"
      "sd s11, 104(a0)\n"
      SAVE_FREGS
      "li a0, 0\n");
}
#endif
#endif
