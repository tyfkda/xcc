#include "string.h"

char *strtok(char *src, const char *delim) {
  static char *ptr;
  return strtok_r(src, delim, &ptr);
}
