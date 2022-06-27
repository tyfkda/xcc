#include "ctype.h"

int toupper(int c) {
  return ('a' <= c && c <= 'z') ? c - ('a' - 'A') : c;
}
