#include "stdlib.h"
#include "errno.h"
#include "fcntl.h"  // open
#include "stdbool.h"
#include "stdint.h"  // uint32_t
#include "string.h"  // strlen
#include "sys/random.h"

int mkstemps(char *tmpl, int suffixlen) {
#define LETTERS (10 + 26 + 26)
  static const char kLetters[LETTERS] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  const int LEN = 6;
  const uint32_t RNDMAX = ((1UL << 31) / LETTERS) * LETTERS;

  static bool initialized;
  static unsigned short xsubi[3] = {11, 222, 3333};
  if (!initialized) {
    initialized = getrandom(xsubi, sizeof(xsubi), 0) == sizeof(xsubi);
  }

  size_t len = strlen(tmpl);
  if (len < (size_t)(LEN + suffixlen)) {
    errno = EINVAL;
    return -1;
  }
  char *p = &tmpl[len - suffixlen - LEN];
  for (int j = 0; j < LEN; ++j) {
    if (*p != 'X') {
      errno = EINVAL;
      return -1;
    }
    uint32_t r;
    do {
      r = nrand48(xsubi);
    } while (r >= RNDMAX);
    *p++ = kLetters[r % LETTERS];
  }

  return open(tmpl, O_RDWR | O_CREAT | O_EXCL | O_TRUNC, S_IRUSR | S_IWUSR);
#undef LETTERS
}
