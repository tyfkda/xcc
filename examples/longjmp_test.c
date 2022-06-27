// Longjmp test
//
// Compile:
//   $ ./xcc -olongjmp_test examples/longjmp_test.c
//
// Run:
//   $ ./longjmp_test  #=> 123

#include <setjmp.h>
#include <stdio.h>

jmp_buf env;

void func(void) {
  longjmp(env, 123);
}

int main() {
  int result;
  if ((result = setjmp(env)) == 0) {
    func();
    printf("never reaches here\n");
    return 1;
  } else {
    printf("%d\n", result);
  }
  return 0;
}
