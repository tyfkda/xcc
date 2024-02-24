#include "string.h"

int strerror_r(int no, char *dst, size_t n) {
  strncpy(dst, strerror(no), n);
  return 0;
}
