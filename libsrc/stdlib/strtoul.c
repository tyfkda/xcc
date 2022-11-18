#include "stdlib.h"

extern unsigned long strtoul_sub(const char *p, char **pp, int base);

unsigned long strtoul(const char *p, char **pp, int base) {
  return strtoull(p, pp, base);
}
