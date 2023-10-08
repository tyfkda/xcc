#include <stdio.h>

int x;

extern void sub(void);

int main(void) {
  x = 1234;
  sub();
  printf("%d\n", x);
  return 0;
}
