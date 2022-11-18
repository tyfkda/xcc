#include "stdlib.h"

long strtol(const char *p, char **pp, int base) {
  return strtoll(p, pp, base);
}
