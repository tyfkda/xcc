// Echo : Handle command line arguments
//
// Compile:
//   $ ./xcc -oecho examples/echo.c
//
// Run:
//   $ ./echo foo bar baz  #=> foo bar baz

#include "../lib/crt0.c"
#include "util.c"

int main(int argc, char **argv) {
  for (int i = 1; i < argc; ++i) {
    if (i > 1)
      puts(" ");
    puts(argv[i]);
  }
  puts("\n");
  return 0;
}
