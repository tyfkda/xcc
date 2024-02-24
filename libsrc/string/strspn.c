#include "string.h"

size_t strspn(const char *s, const char *accept) {
  size_t n;
  for (n = 0; s[n] != '\0' && strchr(accept, s[n]) != NULL; ++n)
    ;
  return n;
}
