#include "math.h"
#include "stdint.h"  // int64_t
#include "_ieee.h"

#ifndef __NO_FLONUM
double copysign(double x, double f) {
#if defined(__wasm)
#define S(x)   S_(x)
#define S_(x)  #x
#define OP_LOCAL_GET      32   // 0x20
#define OP_F64_COPYSIGN   166  // 0xa6
  __asm volatile(
      S(OP_LOCAL_GET) ",0,"  // local.get 0
      S(OP_LOCAL_GET) ",1,"  // local.get 1
      S(OP_F64_COPYSIGN));   // f64.copysign
#elif defined(__riscv) && !defined(__GNUC__)
  __asm volatile("fsgnj.d fa0, fa0, fa1");
#else
  union { double d; int64_t q; } u;
  u.d = x;
  u.q = (u.q & ~SIGN_MASK) | ((uint64_t)signbit(f) << SIGN_POS);
  return u.d;
#endif
}
#endif
