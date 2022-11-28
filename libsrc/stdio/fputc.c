#include "stdio.h"

#include "./_file.h"

int fputc(int c, FILE* fp) {
  fp->wbuf[fp->wp++] = c;
  if (fp->wp >= sizeof(fp->wbuf) ||
      (c == '\n' && !(fp->flag & FF_BINARY))) {
    if (fflush(fp) == EOF)
      return EOF;
  }
  return c;
}
