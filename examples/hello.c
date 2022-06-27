// Hello world!
//
// Compile:
//   $ ./xcc -ohello examples/hello.c
//
// Run:
//   $ ./hello  #=> Hello, world!

#include <unistd.h>

int main() {
  write(1, "Hello, world!\n", 14);
  return 0;
}
