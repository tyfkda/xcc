#include "string.h"

char *stpncpy(char *dst, const char *src, size_t n) {
  for (; n > 0 && (*dst++ = *src++) != '\0'; --n)
    ;
  return dst;
}
