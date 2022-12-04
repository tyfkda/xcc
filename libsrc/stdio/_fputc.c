#include "stdio.h"

#include "./_file.h"

int _fputc(int c, FILE* fp) {
  if (fp->wp < fp->ws) {
    fp->wbuf[fp->wp++] = c;
    if (fp->wp >= fp->ws ||
        (c == '\n' && !(fp->flag & FF_BINARY))) {
      if (fflush(fp) == EOF)
        return EOF;
    }
  } else {
    // TODO: ?
  }
  return c;
}
