#include "stdio.h"

#include "./_file.h"

int fflush(FILE *fp) {
  return (fp->flush)(fp);
}
