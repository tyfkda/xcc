int strlen(char *s) {
  char *p;
  for (p = s; *p != '\0'; ++p)
    ;
  return p - s;
}

void puts(char *s) {
  _write(1, s, strlen(s));
}

void putdeci(long x) {
  char s[16];
  char *p = s + 16;

  int minus = 0;
  if (x < 0) {
    x = -x;
    minus = 1;
  }

  for (; x != 0; x = x / 10)
    *(--p) = (x % 10) + '0';
  if (minus)
    *(--p) = '-';

  _write(1, p, (s + 16) - p);
}
