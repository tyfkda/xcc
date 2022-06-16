#include "string.h"

int strcmp(const char *p, const char *q) {
  while (*p != '\0' && *p == *q) {
    ++p;
    ++q;
  }
  return (int)*(unsigned char*)p - (int)*(unsigned char*)q;
}
