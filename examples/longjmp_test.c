// Longjmp test
//
// Compile:
//   $ ./xcc -olongjmp_test examples/longjmp_test.c lib/setjmp.c
//
// Run:
//   $ ./longjmp_test  #=> 123

#include <setjmp.h>

#include "../lib/crt0.c"
#include "example_util.c"

jmp_buf env;

void func(void) {
  longjmp(env, 123);
}

int main() {
  int result;
  if ((result = setjmp(env)) == 0) {
    func();
    putstr("never reaches here\n");
  } else {
    putdeci(result);
    putstr("\n");
  }
  return 0;
}
