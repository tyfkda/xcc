#include "stdlib.h"

#ifndef __NO_FLONUM
long double strtold(const char* restrict p, char** restrict pp) {
  // TODO: implement.
  return strtod(p, pp);
}
#endif
