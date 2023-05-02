#include "stdlib.h"

int mkstemp(char *tmpl) {
  return mkstemps(tmpl, 0);
}
