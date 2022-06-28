#include "stdio.h"

int snprintf(char *out, size_t n, const char *fmt, ...) {
  va_list ap;
  int len;
  va_start(ap, fmt);
  len = vsnprintf(out, n, fmt, ap);
  va_end(ap);
  return len;
}
