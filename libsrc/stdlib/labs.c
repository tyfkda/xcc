#include "stdlib.h"

long labs(long x) {
  return x >= 0 ? x : -x;
}
