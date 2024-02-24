#include "string.h"

size_t strcspn(const char *s, const char *reject) {
  size_t n;
  for (n = 0; s[n] != '\0' && strchr(reject, s[n]) == NULL; ++n)
    ;
  return n;
}
