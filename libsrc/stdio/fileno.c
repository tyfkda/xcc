#include "stdio.h"

#include "./_file.h"

int fileno(FILE *fp) {
  return fp->fd;
}
