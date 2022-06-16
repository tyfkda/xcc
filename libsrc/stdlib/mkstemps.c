#include "stdlib.h"
#include "errno.h"
#include "fcntl.h"  // open
#include "stdint.h"  // uint32_t
#include "string.h"  // strlen

extern uint32_t xor64(void);

int mkstemps(char *template, int suffixlen) {
#define LETTERS (10 + 26 + 26)
  static const char kLetters[LETTERS] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  const int LEN = 6;
  const uint32_t RNDMAX = ((1UL << 32) / LETTERS) * LETTERS;

  size_t len = strlen(template);
  if (len < LEN + suffixlen) {
    errno = EINVAL;
    return -1;
  }
  char *p = &template[len - suffixlen - LEN];
  for (int j = 0; j < LEN; ++j) {
    if (*p != 'X') {
      errno = EINVAL;
      return -1;
    }
    uint32_t r;
    do {
      r = xor64();
    } while (r >= RNDMAX);
    *p++ = kLetters[r % LETTERS];
  }

  return open(template, O_WRONLY | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
#undef LETTERS
}
