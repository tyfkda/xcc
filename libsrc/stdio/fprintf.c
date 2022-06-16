#include "stdio.h"

int fprintf(FILE *fp, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int len = vfprintf(fp, fmt, ap);
  va_end(ap);
  return len;
}
