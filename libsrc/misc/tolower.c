#include "ctype.h"

int tolower(int c) {
  return ('A' <= c && c <= 'Z') ? c + ('a' - 'A') : c;
}
