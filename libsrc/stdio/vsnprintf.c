#include "stdio.h"
#include "unistd.h"
#include "stdarg.h"

int vsnprintf(char *out, size_t n, const char *fmt, va_list ap) {
  int result = 0;
  char tmp[1];
  if (n <= 0) {
    out = tmp;
    n = sizeof(tmp);
  }
  FILE *fp = fmemopen(out, n, "w");
  if (fp != NULL) {
    result = vfprintf(fp, fmt, ap);
    fclose(fp);
  }
  return result;
}
