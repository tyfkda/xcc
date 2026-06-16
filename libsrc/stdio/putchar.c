#include "stdio.h"

#include "./_file.h"
// gcc replaces `printf("%c", c)` into `putchar(c)` implicitly.
#undef putchar

// Result: Success => c, Failure => EOF
int putchar(int c) {
  return _fputc(c, stdout);
}
