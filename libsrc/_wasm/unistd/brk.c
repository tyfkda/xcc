#include "unistd.h"
#include "stdio.h"  // EOF

extern size_t __heap_base;
static void *curbrk = &__heap_base;
#define CURBRK  curbrk

#define HEAP_ALIGN  (8)
#define MEMORY_PAGE_BIT  (16)
#define MEMORY_INDEX  (0)

static inline void _growTo(void *ptr) {
  size_t page = (((size_t)ptr) + ((1 << MEMORY_PAGE_BIT) - 1)) >> MEMORY_PAGE_BIT;
  size_t count = __builtin_wasm_memory_size(MEMORY_INDEX);
  if (page > count) {
    const size_t grow = page - count;
    __builtin_wasm_memory_grow(MEMORY_INDEX, grow);
  }
}

int brk(void *addr) {
  if (addr <= curbrk)
    return EOF;
  void *p = (void*)((((intptr_t)addr) + (HEAP_ALIGN - 1)) & -HEAP_ALIGN);
  curbrk = p;
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
