#include "stdio.h"
#include "string.h"

// Result: Success => positive value, Failure => EOF
int puts(const char *s) {
  // gcc replaces `printf("%s\n", s);` into `puts(s)` so fail with infinite loop.
  int result = fputs(s, stdout);
  if (result >= 0)
    result = putchar('\n');
  return result;
}
