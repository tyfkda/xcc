#if defined(__XV6)
void _start(void) {
  __asm("call main");
  __asm("mov %eax, %edi");
  __asm("jmp exit");
}

void exit(int code) {
  __asm("mov $2, %eax");  // SYS_exit
  __asm("int $64");
}

#elif defined(__linux__)
void _start(void) {
  __asm(" mov (%rsp), %rdi\n"
        " lea 8(%rsp), %rsi\n"
        " call main\n"
        " mov %eax, %edi\n"
        " jmp exit");
}

void exit(int code) {
  __asm(" mov $60, %eax\n"  // __NR_exit
        " syscall");
}

#else
#error Target not supported
#endif
