#include "ctype.h"

int iscntrl(int c) {
  return (0 <= c && c <= 0x1f) || c == 0x7f;
}
