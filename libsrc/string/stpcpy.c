#include "string.h"

char *stpcpy(char *dst, const char *src) {
  while ((*dst++ = *src++) != '\0')
    ;
  return dst;
}
