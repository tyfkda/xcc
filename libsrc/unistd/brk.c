#include "unistd.h"
#include "stdio.h"  // EOF

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if defined(__linux__)
#include "_syscall.h"

static void *_brk(void *addr) {
  void *ret;
  SYSCALL_RET(__NR_brk, ret);
  return ret;
}
#elif defined(__APPLE__)
extern void *_brk(void *addr);
#endif

static char *curbrk;
#define CURBRK  curbrk

int brk(void *addr) {
  void *result = _brk(addr);
  curbrk = result;
  return result < addr ? EOF : 0;
}

void *sbrk(intptr_t increment) {
  char *p = CURBRK;
  if (p == NULL) {
    if (brk(NULL) < 0)
      return (void*)-1;
    p = CURBRK;
  }
  char *next = p + increment;
  if (brk(next) < 0)
    return (void*)-1;
  return p;
}
