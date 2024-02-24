#include "string.h"

char *strtok_r(char *src, const char *delim, char **ptr) {
  if (src != NULL)
    *ptr = src;
  *ptr += strspn(*ptr, delim);
  if (**ptr == '\0')
    return NULL;
  char *r = *ptr;
  size_t n = strcspn(r, delim);
  r[n] = '\0';
  *ptr += n + 1;
  return r;
}
