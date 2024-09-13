#include "stdlib.h"

#define MAX  (8)

typedef void (*AtexitFunc)(void);

static AtexitFunc buf[MAX];
static int count;

int atexit(void (*func)(void)) {
  if (count >= MAX)
    return 1;
  buf[count++] = func;
  return 0;
}

void __atexit_call(void) {
  for (AtexitFunc *p = &buf[count]; p > buf; )
    (*(--p))();
}
