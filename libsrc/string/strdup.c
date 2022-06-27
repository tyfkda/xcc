#include "string.h"
#include "stdlib.h"  // malloc

char *strdup(const char *str) {
  return strndup(str, strlen(str));
}
