#include "string.h"

size_t strlen(const char *s) {
  const char *p;
  for (p = s; *p != '\0'; ++p)
    ;
  return p - s;
}
