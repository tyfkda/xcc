#include "string.h"
#include "ctype.h"  // tolower

int strcasecmp(const char *p, const char *q) {
  for (;; ++p, ++q) {
    int c1 = tolower(*(unsigned char*)p);
    int c2 = tolower(*(unsigned char*)q);
    int d = c1 - c2;
    if (d != 0)
      return d;
    if (c1 == 0)
      break;
  }
  return 0;
}
