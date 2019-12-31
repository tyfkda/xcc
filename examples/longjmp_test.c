#include <setjmp.h>

#include "../lib/crt0.c"
#include "util.c"

jmp_buf env;

void func(void) {
  longjmp(env, 123);
}

int main() {
  int result;
  if ((result = setjmp(env)) == 0) {
    func();
    puts("never reaches here\n");
  } else {
    putdeci(result);
    puts("\n");
  }
  return 0;
}
