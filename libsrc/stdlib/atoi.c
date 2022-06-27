#include "stdlib.h"

int atoi(const char *s) {
  int n = 0;
  for (; '0' <= *s && *s <= '9'; ++s)
    n = n * 10 + (*s - '0');
  return n;
}
