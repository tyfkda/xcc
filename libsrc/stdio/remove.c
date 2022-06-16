#include "stdio.h"
#include "unistd.h"

int remove(const char *fn) {
  return unlink(fn);
}
