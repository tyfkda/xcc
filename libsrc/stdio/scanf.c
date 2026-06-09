#include "stdio.h"
#include "stdarg.h"

int scanf(const char *restrict format, ...) {
  va_list ap;
  va_start(ap, format);
  int result = vfscanf(stdin, format, ap);
  va_end(ap);
  return result;
}
