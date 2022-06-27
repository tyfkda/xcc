// Fib : Calculate fibonacci (30)
//
// Compile:
//   $ ./xcc -ofib examples/fib.c
//
// Run:
//   $ ./fib  #=> 832040

#include <stdio.h>

int fib(int n) {
  if (n < 2)
    return n;
  else
    return fib(n - 1) + fib(n - 2);
}

int main() {
  printf("%d\n", fib(30));
  return 0;
}
