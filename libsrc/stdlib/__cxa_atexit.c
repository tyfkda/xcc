#include "stdlib.h"
#include "_exit.h"

#define MAX  (8)

typedef struct {
  void (*dtor)(void*);
  void *arg;
  void *dso_handle;
} CxaAtexitBuf;

static CxaAtexitBuf buf[MAX];
static int count;

void *__dso_handle;

int __cxa_atexit(void (*func)(void*), void *arg, void *d) {
  if (count >= MAX)
    return 1;
  CxaAtexitBuf *p = &buf[count++];
  p->dtor = func;
  p->arg = arg;
  p->dso_handle = d;
  return 0;
}

static void call_cxa_atexit_funcs(void) {
  for (CxaAtexitBuf *p = &buf[count]; p > buf; ) {
    --p;
    (*p->dtor)(p->arg);
  }
}

__attribute__((constructor))
static void register_atexit(void) {
  static OnExitChain chain = {NULL, call_cxa_atexit_funcs};
  chain.next = __on_exit_chain;
  __on_exit_chain = &chain;
}
