#include "stdio.h"
#include "stdlib.h"  // free
#include "unistd.h"  // close

#include "_file.h"

int fclose(FILE *fp) {
  if (fp == NULL || fp->fd < 0)
    return EOF;
  close(fp->fd);
  fp->fd = -1;
  free(fp);
  return 0;
}
