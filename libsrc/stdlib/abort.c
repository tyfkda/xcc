#include "stdlib.h"
#include "signal.h"
#include "unistd.h"

#if !defined(__wasm)
void abort(void) {
  raise(SIGABRT);
}
#endif
