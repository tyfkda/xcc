#include "ctype.h"

int isgraph(int c) {
  return 0x21 <= c && c <= 0x7e;
}
