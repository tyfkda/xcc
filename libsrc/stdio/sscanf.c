#include "stdio.h"
#include "stdarg.h"

int sscanf(const char *restrict buffer, const char *restrict format, ...) {
  va_list ap;
  va_start(ap, format);
  int result = vsscanf(buffer, format, ap);
  va_end(ap);
  return result;
}
