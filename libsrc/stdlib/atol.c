#include "stdlib.h"

long atol(const char *s) {
  return strtol(s, NULL, 10);
}
