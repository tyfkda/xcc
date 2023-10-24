#include "stdio.h"

#include "_file.h"

int fclose(FILE *fp) {
  if (fp == NULL)
    return EOF;
  return (fp->iof->close)(fp);
}
