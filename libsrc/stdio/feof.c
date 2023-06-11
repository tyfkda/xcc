#include "stdio.h"

#include "_file.h"

int feof(FILE *fp) {
  return (fp->flag & FF_EOF) ? 1 : 0;
}
