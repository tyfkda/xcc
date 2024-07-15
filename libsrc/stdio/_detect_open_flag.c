#include "stdio.h"

#include "fcntl.h"  // O_ACCMODE, etc.

int _detect_open_flag(const char *mode) {
  enum Chr {
    RWMASK = 3,
    R = 1 | (1 << 5),
    W = 2 | (1 << 5),
    P = 1 << 2,
    MASK = 7,
    A = 1 << 3,
    B = 1 << 4,
  };
  enum Err {
    OK,
    CONFLICT,
    ILLEGAL_CHR,
  };

  int chr = 0, c = 0;
  enum Err err = OK;
  for (const char *p = mode; (c = *p) != '\0'; ++p) {
    switch (c) {
    case 'r':  c = R; break;
    case 'w':  c = W; break;
    case 'a':  c = W | A; break;
    case '+':  c = P; break;
    case 'b':  c = B; break;
    default:  err = ILLEGAL_CHR; break;
    }
    if (err != OK || ((chr & c) != 0 && (err = CONFLICT, 1)))
      break;
    chr |= c;
  }
  switch (err) {
  case OK: default:
    break;
  case CONFLICT:
    fprintf(stderr, "conflict: %c\n", c);
    return -1;
  case ILLEGAL_CHR:
    fprintf(stderr, "illegal character: %c\n", c);
    return -1;
  }
  if ((chr & RWMASK) == 0) {
    fprintf(stderr, "r|w|a required\n");
    return -1;
  }

  static const int kTable[] = {
    [R & MASK] = O_RDONLY,
    [W & MASK] = O_WRONLY | O_CREAT,
    [(R | P) & MASK] = O_RDWR,
    [(W | P) & MASK] = O_RDWR | O_CREAT,
  };
  int flag = kTable[chr & MASK];
  if ((chr & RWMASK) == (W & RWMASK))
    flag |= (chr & A) ? O_APPEND : O_TRUNC;
  return flag;
}
