#include "string.h"

char *strncat(char *dst, const char *src, size_t n) {
  char *os = dst;
  dst += strlen(dst);
  for (; n > 0 && *src != '\0'; --n)
    *dst++ = *src++;
  *dst = '\0';
  return os;
}
