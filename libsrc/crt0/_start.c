char **environ;

#if defined(__XV6)
void _start(void) {
  __asm("call main\n"
        "mov %eax, %edi\n"
        "jmp exit");
}

#elif defined(__linux__)
void _start(void) {
#if defined(__x86_64__)
  __asm("mov (%rsp), %rdi\n"
        "lea 8(%rsp), %rsi\n"
        "lea 8(%rsi, %rdi, 8), %rdx\n"
#if 0
        "mov %rdx, environ(%rip)\n"
#else
        "lea environ(%rip), %rax\n"
        "mov %rdx, (%rax)\n"
#endif
        "call main\n"
        "mov %eax, %edi\n"
        "jmp exit");
#elif defined(__aarch64__)
  __asm("mov x0, x1\n"
        "mov x1, x2\n"
        "mov x2, x3\n"
        "adrp x3, environ\n"
        "add x3, x3, :lo12:environ\n"
        "str x2, [x3]\n"
        "bl main\n"
        "b exit");
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
