#include "string.h"
#include "stdlib.h"  // malloc

char *strndup(const char *str, size_t size) {
  char *dup = malloc(size + 1);
  memcpy(dup, str, size);
  dup[size] = '\0';
  return dup;
}
