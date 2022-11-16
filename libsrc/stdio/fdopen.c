#include "stdio.h"
#include "stdlib.h"  // malloc, realloc
#include "string.h"

#include "_file.h"
#include "_fileman.h"

#define INITIAL_CAPACITY  (4)

static void add_opened(FILE *fp) {
  extern FILEMAN __fileman;

  if (__fileman.length >= __fileman.capacity) {
    int ncapa = __fileman.capacity > 0 ? __fileman.capacity << 1 : INITIAL_CAPACITY;
    FILE **buf = realloc(__fileman.opened, ncapa * sizeof(*__fileman.opened));
    if (buf == NULL) {
      // TODO:
      fprintf(stderr, "Out of memory\n");
      exit(1);
    }
    __fileman.opened = buf;
    __fileman.capacity = ncapa;
  }
  __fileman.opened[__fileman.length++] = fp;
}

FILE *fdopen(int fd, const char *mode) {
  // TODO: Validate fd.

  FILE *fp = malloc(sizeof(*fp));
  if (fp != NULL) {
    fp->fd = fd;
    fp->wp = 0;

    int flag = 0;
    if (strchr(mode, 'b') != NULL)
      flag |= FF_BINARY;
    fp->flag = flag;

    add_opened(fp);
  }
  return fp;
}
