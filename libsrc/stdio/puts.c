#include "stdio.h"
#include "string.h"

int puts(const char *s) {
  // gcc replaces `printf("%s\n", s);` into `puts(s)` so fail with infinite loop.
  size_t len = strlen(s);
  return fwrite(s, 1, len, stdout) == len &&
      putchar('\n') == 0 ? 0 : EOF;
}
