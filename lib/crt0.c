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

#elif defined(__linux__) || defined(__APPLE__)
void _start(void) {
  __asm("mov (%rsp), %rdi");
  __asm("lea 8(%rsp), %rsi");
  __asm("call main");
  __asm("mov %eax, %edi");
  __asm("jmp exit");
}

void exit(int code) {
  __asm("mov $60, %eax");  // __NR_exit
  __asm("syscall");
}

#else
#error Target not supported
#endif
