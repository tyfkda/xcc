#include "stdio.h"

#include "_file.h"

#include "unistd.h"

FILE *open_memstream(char **ptr, size_t *sizeloc) {
  FILE *fp = fmemopen(NULL, 0, "w");
  if (fp != NULL) {
    fp->flag |= FF_GROWMEM;
    fp->pmem = ptr;
    fp->psize = sizeloc;
  }
  return fp;
}
