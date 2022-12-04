#include "stdio.h"

#if defined(__GNUC__)
#include "./_file.h"
// gcc replaces `printf("%c", c)` into `putchar(c)` implicitly.
#undef putchar
int putchar(int c) {
  return FPUTC(c, stdout);
}
#endif
