#include "stdlib.h"  // exit

#include "../wasi.h"

extern void __wasm_call_ctors(void);

void _start(void) {
  extern int __main_void(void);

  __wasm_call_ctors();

  int ec = __main_void();
  exit(ec);
#undef main
}
