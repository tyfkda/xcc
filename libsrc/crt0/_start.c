#if defined(__XV6)
void _start(void) {
  __asm("call main\n"
        "mov %eax, %edi\n"
        "jmp exit");
}

#elif defined(__WASM)

int _start(int argc, char *argv[]) {
  extern int main(int, char**);
  return main(argc, argv);
}

#elif defined(__linux__)
void _start(void) {
  __asm("mov (%rsp), %rdi\n"
        "lea 8(%rsp), %rsi\n"
        "call main\n"
        "mov %eax, %edi\n"
        "jmp exit");
}

#elif defined(__APPLE__)

// Use libc.

extern void exit(int code);

#else
#error Target not supported
#endif
