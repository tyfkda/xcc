#include "stdlib.h"
#include "_exit.h"

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

static void call_atexit_funcs(void) {
  for (AtexitFunc *p = &buf[count]; p > buf; ) {
    --p;
    (*p)();
  }
}

__attribute__((constructor))
static void register_atexit(void) {
  static OnExitChain chain = {NULL, call_atexit_funcs};
  chain.next = __on_exit_chain;
  __on_exit_chain = &chain;
}
