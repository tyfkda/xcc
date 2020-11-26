#include "test.h"

#ifdef __x86_64__
int asm_fn1(void) {
  __asm("mov $50, %rax");
}

int asm_fn2(void) {
  __asm("mov $55, %rax");
}

int main() {
  ASSERT(50, asm_fn1());
  ASSERT(55, asm_fn2());

  printf("OK\n");
  return 0;
}
#else
int main() { return 0; }
#endif
