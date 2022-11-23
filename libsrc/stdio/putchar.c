#include "stdio.h"

#if defined(__GNUC__)
// gcc replaces `printf("%c", c)` into `putchar(c)` implicitly.
#undef putchar
int putchar(int c) {
  return fputc(c, stdout);
}
#endif
