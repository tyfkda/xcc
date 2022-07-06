#include "stdio.h"
#include "stdlib.h"  // malloc

#include "_file.h"

FILE *fdopen(int fd, const char *mode) {
  // TODO: Validate fd and mode.

  FILE *fp = malloc(sizeof(*fp));
  if (fp != NULL) {
    fp->fd = fd;
  }
  return fp;
}
