#include "string.h"

char *strncpy(char *dst, const char *src, size_t n) {
  char *os = dst;
  for (; n > 0 && (*dst++ = *src++) != '\0'; --n)
    ;
  return os;
}
