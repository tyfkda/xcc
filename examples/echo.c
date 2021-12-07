// Echo : Handle command line arguments
//
// Compile:
//   $ ./xcc -oecho examples/echo.c
//
// Run:
//   $ ./echo foo bar baz  #=> foo bar baz

#include "../lib/crt0.c"
#include "example_util.c"

int main(int argc, char **argv) {
  for (int i = 1; i < argc; ++i) {
    if (i > 1)
      putstr(" ");
    putstr(argv[i]);
  }
  putstr("\n");
  return 0;
}
