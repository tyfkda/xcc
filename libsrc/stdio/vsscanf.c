#include "stdio.h"
#include "stdarg.h"
#include "string.h"

int vsscanf(const char *restrict buffer, const char *restrict format, va_list ap) {
  size_t len = strlen(buffer);
  FILE *fp = fmemopen((void*)buffer, len, "r");
  int result = 0;
  if (fp != NULL) {
    result = vfscanf(fp, format, ap);
    fclose(fp);
  }
  return result;
}
