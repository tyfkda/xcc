#include "assert.h"
#include "stdio.h"
#include "stdlib.h"

int __assert_failed(const char *fn, int lineno) {
  fprintf(stderr, "Assert failed at %s(%d)\n", fn, lineno);
  exit(1);
}
