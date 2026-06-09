#include "stdio.h"
#include "stdarg.h"

int fscanf(FILE *restrict fp, const char *restrict format, ...) {
  va_list ap;
  va_start(ap, format);
  int result = vfscanf(fp, format, ap);
  va_end(ap);
  return result;
}
