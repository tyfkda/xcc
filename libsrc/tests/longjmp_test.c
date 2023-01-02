// Longjmp test
//
// Compile:
//   $ ./xcc -olongjmp_test libsrc/tests/longjmp_test.c
//
// Run:
//   $ ./longjmp_test || echo fail

#include <setjmp.h>

jmp_buf env;

void func(void) {
  longjmp(env, 123);
}

int main() {
  int result;
  if ((result = setjmp(env)) == 0) {
    func();
    // never reaches here
    return 1;
  } else {
    return result == 123 ? 0 : 2;
  }
}
