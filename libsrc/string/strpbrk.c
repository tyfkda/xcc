#include "string.h"

char *strpbrk(const char *s, const char *accept) {
  const char *r = s + strcspn(s, accept);
  if (*r == '\0')
    return NULL;
  return (char*)r;
}
