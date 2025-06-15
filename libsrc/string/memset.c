#include "string.h"

void *memset(void *buf, int val, size_t size) {
#if defined(__wasm)
#define S(x)   S_(x)
#define S_(x)  #x
#define OP_LOCAL_GET      32   // 0x20
#define OP_0xFC           252  // 0xfc
#define OPFC_MEMORY_FILL  11   // 0x0b
  __asm(
      S(OP_LOCAL_GET) ",0,"  // local.get 0
      S(OP_LOCAL_GET) ",1,"  // local.get 1
      S(OP_LOCAL_GET) ",2,"  // local.get 2
      S(OP_0xFC) "," S(OPFC_MEMORY_FILL) ",0");  // memory.fill
#else
  unsigned char *p = buf;
  unsigned char v = val;
  for (size_t i = 0; i < size; ++i)
    *p++ = v;
#endif
  return buf;
}
