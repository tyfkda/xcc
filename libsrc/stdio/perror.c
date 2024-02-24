#include "stdio.h"
#include "string.h"
#include "errno.h"

void perror(const char *msg) {
  int no = errno;
  if (msg != NULL && *msg != '\0')
    fprintf(stderr, "%s: ", msg);
  fprintf(stderr, "%s\n", strerror(no));
}
