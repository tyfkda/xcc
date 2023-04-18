#include "unistd.h"
#include "stdio.h"  // EOF

extern char *__curbrk;
#define CURBRK  __curbrk

#define HEAP_ALIGN  (8)
#define MEMORY_PAGE_BIT  (16)

static void _growTo(void *ptr) {
  size_t page = (((size_t)ptr) + ((1 << MEMORY_PAGE_BIT) - 1)) >> MEMORY_PAGE_BIT;
  size_t count = __builtin_memory_size();
  if (page > count) {
    const size_t grow = page - count;
    __builtin_memory_grow(grow);
  }
}

int brk(void *addr) {
  if (addr <= __curbrk)
    return EOF;
  void *p = (void*)((((intptr_t)addr) + (HEAP_ALIGN - 1)) & -HEAP_ALIGN);
  __curbrk = p;
  _growTo(p);
  return 0;
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
