#include "string.h"

void *memcpy(void *dst, const void *src, size_t n) {
#if defined(__WASM)
#define S(x)   S_(x)
#define S_(x)  #x
#define OP_LOCAL_GET      32   // 0x20
#define OP_0xFC           252  // 0xfc
#define OPFC_MEMORY_COPY  10   // 0x0a
  __asm(
      S(OP_LOCAL_GET) ",0,"  // local.get 0
      S(OP_LOCAL_GET) ",1,"  // local.get 1
      S(OP_LOCAL_GET) ",2,"  // local.get 2
      S(OP_0xFC) "," S(OPFC_MEMORY_COPY) ",0,0");  // memory.copy
#else
  const char *s = src;
  char *d = dst;
  while (n-- > 0)
    *d++ = *s++;
#endif
  return dst;
}
