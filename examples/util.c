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

void write(int fd, const char *str, long len) {
  __hexasm(0xb8, 0x10, 0x00, 0x00, 0x00);  // mov $SYS_write(=16), %eax
  __hexasm(0xcd, 0x40);                    // int $64
}

#elif defined(__linux__)
void _start(void) {
  __hexasm(0x48, 0x8b, 0x7c, 0x24, 0);  // mov 0(%rsp),%rdi
  __hexasm(0x48, 0x8d, 0x74, 0x24, 8);  // lea 8(%rsp),%rsi
  __hexasm(0xe8, __rel32("main"));      // call main
  __hexasm(0x89, 0xc7);                 // mov %eax,%edi
  __hexasm(0xe9, __rel32("exit"));      // call exit
}

void exit(int code) {
  __hexasm(0xb8, 0x3c, 0x00, 0x00, 0x00);  // mov $__NR_exit(=60), %eax
  __hexasm(0x0f, 0x05);                    // syscall
}

void write(int fd, const char *str, long len) {
  __hexasm(0xb8, 0x01, 0x00, 0x00, 0x00);  // mov $__NR_write(=1), %eax
  __hexasm(0x0f, 0x05);                    // syscall
}

#else
#error Target not supported
#endif

int strlen(char *s) {
  char *p;
  for (p = s; *p != '\0'; ++p)
    ;
  return p - s;
}

void puts(char *s) {
  write(1, s, strlen(s));
}

void putdeci(long x) {
  char s[16];
  char *p = s + 16;

  int minus = 0;
  if (x < 0) {
    x = -x;
    minus = 1;
  }

  do {
    *(--p) = (x % 10) + '0';
    x /= 10;
  } while (x != 0);
  if (minus)
    *(--p) = '-';

  write(1, p, (s + 16) - p);
}
