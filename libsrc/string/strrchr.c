#include "string.h"

char *strrchr(const char *s, int c) {
  char *last = NULL;
  for(; *s != '\0'; ++s)
    if(*s == c)
      last = (char*)s;
  return last;
}
