// Hello world!
//
// Compile:
//   $ ./xcc -ohello examples/hello.c
//
// Run:
//   $ ./hello  #=> Hello, world!

#include "../lib/crt0.c"
#include "example_util.c"

int main() {
  write(1, "Hello, world!\n", 14);
  return 0;
}
