#include "string.h"

char *strcpy(char *dst, const char *src) {
  char *os = dst;
  while ((*dst++ = *src++) != '\0')
    ;
  return os;
}
