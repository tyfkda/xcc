#include "stdbool.h"

bool _parse_sign(const char **pp) {
  const char *p = *pp;
  char c = *p;
  if (c == '+' || c == '-')
    *pp = p + 1;
  return c == '-';
}
