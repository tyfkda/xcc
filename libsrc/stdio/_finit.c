#include "stdio.h"

#include "sys/stat.h"  // fstat
#include "./_file.h"

void _finit(FILE *fp) {
  if ((fp->flag & FF_INITIALIZED))
    return;

  struct stat st;
  int r = fstat(fp->fd, &st);
  if (r == 0) {
    switch (st.st_mode & S_IFMT) {
    case S_IFIFO:
    case S_IFSOCK:
      fp->flag |= FF_FIFO;
      break;
    default:
      break;
    }
  } else {
    // TODO: Retry.
  }
  fp->flag |= FF_INITIALIZED;
}
