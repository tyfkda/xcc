#include "assert.h"
#include "stdio.h"
#include "stdlib.h"

int __assert_failed(const char *assertion, const char *fn, int lineno) {
  fprintf(stderr, "%s:%d: Assertion `%s` failed\n", fn, lineno, assertion);
  exit(1);
}
