#if !defined(__WASM)
#include "stdlib.h"

int mkstemp(char *template) {
  return mkstemps(template, 0);
}
#endif
