#include "stdlib.h"

#define ATEXIT_MAX  (8)

typedef void (*AtexitFunc)(void);

static AtexitFunc __atexit_funcs[ATEXIT_MAX];
static int __atexit_count;

int atexit(void (*func)(void)) {
  int count = __atexit_count;
  if (count >= ATEXIT_MAX)
    return 1;
  __atexit_funcs[count] = func;
  ++__atexit_count;
  return 0;
}

void __atexit_call(void) {
  for (AtexitFunc *p = &__atexit_funcs[__atexit_count]; p > __atexit_funcs; )
    (*(--p))();
}
