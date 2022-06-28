#include "stdio.h"
#include "stdarg.h"

int sprintf(char *out, const char *fmt, ...) {
  va_list ap;
  int len;
  va_start(ap, fmt);
  len = vsnprintf(out, (size_t)-1, fmt, ap);
  va_end(ap);
  return len;
}
