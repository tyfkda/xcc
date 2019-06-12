#if defined(__XV6)
void _start(void) {
  __asm(0xe8, __rel32("main"));     // call main
  __asm(0x89, 0xc7);                // mov %eax,%edi
  __asm(0xe9, __rel32("exit"));     // jmp exit
}

void exit(int code) {
  __asm("mov $2, %eax", 0xb8, 0x02, 0x00, 0x00, 0x00);  // SYS_exit
  __asm("int $64",      0xcd, 0x40);
}

#elif defined(__linux__)
void _start(void) {
#if defined(__XCC)
  __asm("mov 0(%rsp), %rdi", 0x48, 0x8b, 0x7c, 0x24, 0);
  __asm("lea 8(%rsp), %rsi", 0x48, 0x8d, 0x74, 0x24, 8);
  __asm("call main",         0xe8, __rel32("main"));
  __asm("mov %eax, %edi",    0x89, 0xc7);
  __asm("jmp exit",          0xe9, __rel32("exit"));
#else
  __asm("mov 0(%rsp), %rdi\n"
        "lea 8(%rsp), %rsi\n"
        "call main\n"
        "mov %eax, %edi\n"
        "jmp exit");
#endif
}

void exit(int code) {
#if defined(__XCC)
  __asm("mov $60, %eax", 0xb8, 0x3c, 0x00, 0x00, 0x00);  // __NR_exit
  __asm("syscall",       0x0f, 0x05);
#else
  __asm("mov $60, %eax\n"
        "syscall");
#endif
}

#else
#error Target not supported
#endif
