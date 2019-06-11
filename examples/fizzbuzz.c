#include "../lib/crt0.c"
#include "util.c"

int main() {
  int i;
  for (i = 1; i <= 100; ++i) {
    if ((i % 15) == 0) {
      puts("fizzbuzz\n");
    } else if ((i % 5) == 0) {
      puts("buzz\n");
    } else if ((i % 3) == 0) {
      puts("fizz\n");
    } else {
      putdeci(i);
      puts("\n");
    }
  }
  return 0;
}
