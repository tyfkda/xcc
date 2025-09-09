#include "assert.h"

#include "stdio.h"
#include "stdlib.h"

void __assert_fail(const char *assertion, const char *fn, int lineno, const char *func) {
  fprintf(stderr, "%s:%d: %s: Assertion `%s` failed\n", fn, lineno, func, assertion);
  exit(1);
}
