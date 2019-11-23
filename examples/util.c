#if defined(__XV6)
long write(int fd, const char *str, long len) {
  __asm("mov $16, %eax");  // SYS_write
  __asm("int $64");
}

#elif defined(__linux__)
long write(int fd, const char *str, long len) {
  __asm("mov $1, %eax");  // __NR_write
  __asm("syscall");
}

#elif defined(__APPLE__)

// Use libc.

extern long write(int fd, const char *str, long len);

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
