#include "string.h"

char *strchr(const char *s, int c) {
  for (; *s != '\0'; ++s)
    if (*s == c)
      return (char*)s;
  return 0;
}
