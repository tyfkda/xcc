#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void sieve(int n) {
  uint8_t *notprime = calloc(sizeof(*notprime), n);
  if (notprime == NULL) {
    exit(1);
  }

  for (int i = 2; i < n; ++i) {
    if (notprime[i])
      continue;
    printf("%d\n", i);
    for (int j = i * i; j < n; j += i)
      notprime[j] = true;
  }
  free(notprime);
}

int main(int argc, char *argv[]) {
  int n = 100;
  if (argc > 1) {
    n = atoi(argv[1]);
    if (n < 2) {
      return 1;
    }
  }
  sieve(n);
  return 0;
}
