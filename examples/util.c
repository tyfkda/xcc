#if defined(__XV6)
void write(int fd, const char *str, long len) {
  __hexasm(0xb8, 0x10, 0x00, 0x00, 0x00);  // mov $SYS_write(=16), %eax
  __hexasm(0xcd, 0x40);                    // int $64
}

#elif defined(__linux__)
void write(int fd, const char *str, long len) {
#if defined(__XCC)
  __hexasm(0xb8, 0x01, 0x00, 0x00, 0x00);  // mov $__NR_write(=1), %eax
  __hexasm(0x0f, 0x05);                    // syscall
#else
  __asm("mov $1, %eax\n"
        "syscall");
#endif
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
