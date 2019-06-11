#if defined(__XV6)
void _start(void) {
  __hexasm(0xe8, __rel32("main"));     // call main
  __hexasm(0x89, 0xc7);                // mov %eax,%edi
  __hexasm(0xe9, __rel32("exit"));     // jmp exit
}

void exit(int code) {
  __hexasm(0xb8, 0x02, 0x00, 0x00, 0x00);  // mov $SYS_exit(=2), %eax
  __hexasm(0xcd, 0x40);                    // int $64
}

#elif defined(__linux__)
void _start(void) {
  __hexasm(0x48, 0x8b, 0x7c, 0x24, 0);  // mov 0(%rsp),%rdi
  __hexasm(0x48, 0x8d, 0x74, 0x24, 8);  // lea 8(%rsp),%rsi
  __hexasm(0xe8, __rel32("main"));      // call main
  __hexasm(0x89, 0xc7);                 // mov %eax,%edi
  __hexasm(0xe9, __rel32("exit"));      // jmp exit
}

void exit(int code) {
  __hexasm(0xb8, 0x3c, 0x00, 0x00, 0x00);  // mov $__NR_exit(=60), %eax
  __hexasm(0x0f, 0x05);                    // syscall
}

#else
#error Target not supported
#endif
