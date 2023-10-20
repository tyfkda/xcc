#include "stdio.h"
#include "stdlib.h"  // malloc, realloc
#include "string.h"

#include "_file.h"
#include "_fileman.h"

#define INITIAL_CAPACITY  (4)

extern FILEMAN __fileman;

static void add_opened(FILE *fp) {
  if (__fileman.length >= __fileman.capacity) {
    int ncapa = __fileman.capacity > 0 ? __fileman.capacity << 1 : INITIAL_CAPACITY;
    FILE **buf = realloc(__fileman.opened, ncapa * sizeof(*__fileman.opened));
    if (buf == NULL) {
      // TODO:
      fputs("Out of memory\n", stderr);
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
    fp->fputc = _fputc;
    fp->fd = fd;
    fp->rp = fp->rs = 0;
    fp->wp = 0;
    fp->wbuf = fp->wwork;
    fp->ws = sizeof(fp->wwork);

    int flag = 0;
    if (strchr(mode, 'b') != NULL)
      flag |= FF_BINARY;
    fp->flag = flag;

    add_opened(fp);
  }
  return fp;
}
