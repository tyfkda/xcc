#include "stdio.h"

int vsprintf(char *buf, const char *fmt, va_list ap) {
  return vsnprintf(buf, -1UL, fmt, ap);
}
