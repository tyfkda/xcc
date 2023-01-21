#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

#ifndef __NO_FLONUM
double copysign(double x, double f) {
#if defined(__WASM)
#define S(x)   S_(x)
#define S_(x)  #x
#define OP_LOCAL_GET      32   // 0x20
#define OP_F64_COPYSIGN   166  // 0xa6
  __asm(
      S(OP_LOCAL_GET) ",0,"  // local.get 0
      S(OP_LOCAL_GET) ",1,"  // local.get 1
      S(OP_F64_COPYSIGN));   // f64.copysign
#else
  return (f >= 0) == (x >= 0) ? x : -x;
#endif
}
#endif
