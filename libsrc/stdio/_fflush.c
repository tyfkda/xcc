#include "stdio.h"

#include "unistd.h"
#include "./_file.h"

int _fflush(FILE *fp) {
  if (fp->flag & FF_WRITE) {
    unsigned int wp = fp->wp;
    if (wp > 0) {
      size_t write_size = write(fp->fd, fp->wbuf, wp);
      if (write_size != (size_t)wp)
        return EOF;
      fp->wp = 0;
    }
  }
  return 0;
}
