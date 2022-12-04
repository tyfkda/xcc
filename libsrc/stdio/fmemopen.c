#include "stdio.h"
#include "stdlib.h"

#include "_file.h"

#define MINCAPA  (16)

static int memputc(int c, FILE* fp) {
  if (fp->wp + 1 >= fp->ws) {
    if (!(fp->flag & FF_GROWMEM)) {
      return c;  // TODO: Check return value.
    }
    int newsiz = fp->ws * 2;
    if (newsiz < MINCAPA)
      newsiz = MINCAPA;
    void *newbuf = realloc(fp->wbuf, newsiz);
    if (newbuf == NULL) {
      // TODO: Raise error.
      return c;
    }
    fp->wbuf = newbuf;
    fp->ws = newsiz;
  }
  fp->wbuf[fp->wp++] = c;
  return c;
}

FILE *fmemopen(void *buf, size_t size, const char *mode) {
  FILE *fp = fdopen(-1, mode);
  if (fp != NULL) {
    fp->fputc = memputc;
    fp->wbuf = buf;
    fp->ws = size;
    fp->flag |= FF_MEMORY;
    fp->pmem = NULL;
    fp->psize = NULL;
  }
  return fp;
}
