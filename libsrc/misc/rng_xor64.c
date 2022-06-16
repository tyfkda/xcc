#include "stdint.h"
#include "stdbool.h"
#include "sys/random.h"

uint32_t xor64(void) {
  static uint64_t seed = 88172645463325252ULL;
  static bool initialized;
  if (!initialized) {
    initialized = getrandom(&seed, sizeof(seed), 0) == sizeof(seed);
  }

  uint64_t x = seed;
  x ^= x << 13;
  x ^= x >> 7;
  x ^= x << 17;
  seed = x;
  return x;
}
