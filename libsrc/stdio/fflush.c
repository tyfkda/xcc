#include "stdio.h"
#include "unistd.h"

#include "_file.h"

int fflush(FILE *fp) {
  int wp = fp->wp;
  if (wp > 0) {
    fp->wp = 0;
    size_t write_size = write(fp->fd, fp->wbuf, wp);
    if (write_size != (size_t)wp)
      return EOF;
  }
  return 0;
}
