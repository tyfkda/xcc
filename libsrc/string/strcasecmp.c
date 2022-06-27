#include "string.h"
#include "ctype.h"  // tolower

int strcasecmp(const char *p, const char *q) {
  for (;; ++p, ++q) {
    unsigned char c1 = *(unsigned char*)p;
    unsigned char c2 = *(unsigned char*)q;
    int d = (int)c1 - (int)c2;
    if (d != 0)
      return d;
    if (c1 == 0)
      break;
  }
  return 0;
}
