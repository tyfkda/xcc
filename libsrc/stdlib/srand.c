#include "stdlib.h"

void srand(unsigned int seed) {
  srand48(seed);
}
