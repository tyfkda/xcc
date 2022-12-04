#include "stdio.h"
#include "unistd.h"

#include "_file.h"

int fflush(FILE *fp) {
  int wp = fp->wp;
  if (fp->flag & FF_MEMORY) {
    if (wp < fp->ws) {
      fp->wbuf[wp] = '\0';
    }
  } else if (wp > 0) {
    size_t write_size = write(fp->fd, fp->wbuf, wp);
    if (write_size != (size_t)wp)
      return EOF;
    fp->wp = 0;
  }
  return 0;
}
