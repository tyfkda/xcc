#include "stdio.h"

#include "_file.h"

#include "unistd.h"

static int memputc(int c, FILE* fp) {
  if (fp->wp + 1 < fp->ws) {
    fp->wbuf[fp->wp++] = c;
  }
  return c;
}

FILE *fmemopen(void *buf, size_t size, const char *mode) {
  FILE *fp = fdopen(-1, mode);
  if (fp != NULL) {
    fp->fputc = memputc;
    fp->wbuf = buf;
    fp->ws = size;
    fp->flag |= FF_MEMORY;
  }
  return fp;
}
