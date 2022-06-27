#include "stdio.h"

void perror(const char *msg) {
  fprintf(stderr, "perror: %s\n", msg);
}
