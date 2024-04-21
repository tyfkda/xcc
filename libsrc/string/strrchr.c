#include "string.h"

char *strrchr(const char *s, int c) {
  char *last = NULL;
  for (;; ++s) {
    if (*s == c)
      last = (char*)s;
    if (*s == '\0')
      return last;
  }
}
