#include "stdlib.h"  // exit

#include "../wasi.h"

extern void _start0(void);

void _start(void) {
  _start0();

  extern int __main_void(void);
  int ec = __main_void();
  exit(ec);
}
