// Echo : Handle command line arguments
//
// Compile:
//   $ ./xcc -oecho examples/echo.c
//
// Run:
//   $ ./echo foo bar baz  #=> foo bar baz

#include <stdio.h>

int main(int argc, char **argv) {
  for (int i = 1; i < argc; ++i) {
    if (i > 1)
      printf(" ");
    printf("%s", argv[i]);
  }
  printf("\n");
  return 0;
}
