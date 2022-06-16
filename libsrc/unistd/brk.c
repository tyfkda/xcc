#include "unistd.h"
#include "stdio.h"  // EOF

#if defined(__linux__)
static void *_brk(void *addr) {
  __asm("mov $12, %eax\n"  // __NR_brk
        "syscall");
}
#endif

#if !defined(__XV6)
static char *curbrk;
int brk(void *addr) {
  void *result = _brk(addr);
  curbrk = result;
  if (result < addr)
    return EOF;
  return 0;
}

void *sbrk(intptr_t increment) {
  char *p = curbrk;
  if (p == NULL)
    p = _brk(NULL);
  char *next = p + increment;
  if (brk(next) < 0)
    return (void*)-1;
  return p;
}
#endif
