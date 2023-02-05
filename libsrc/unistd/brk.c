#include "unistd.h"
#include "stdio.h"  // EOF

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

#if !defined(__XV6)
#if defined(__WASM)
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
#else

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
#endif

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
#endif
