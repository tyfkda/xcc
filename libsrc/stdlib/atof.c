#include "stdlib.h"

#ifndef __NO_FLONUM
double atof(const char* p) {
  return strtod(p, NULL);
}
#endif
