#include "stdlib.h"
#include "ctype.h"  // isspace
#include "stdbool.h"

int atoi(const char *s) {
  for (; isspace(*s); ++s)
    ;

  bool negative = false;
  char c = *s;
  switch (c) {
  case '-':
    negative = true;
    // Fallthrough
  case '+':
    ++s;
    break;
  }

  int n = 0;
  for (; '0' <= *s && *s <= '9'; ++s)
    n = n * 10 + (*s - '0');
  return negative ? -n : n;
}
