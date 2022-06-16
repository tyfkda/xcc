#include "stdio.h"
#include "unistd.h"

int vfprintf(FILE *fp, const char *fmt, va_list ap) {
  // TODO: directly output to fd, not use vsnprintf.
  char buf[1024];
  int len = vsnprintf(buf, sizeof(buf), fmt, ap);
  return write(fileno(fp), buf, len);
}
