#include "stdio.h"
#include "unistd.h"  // write

#include "./_file.h"

size_t fwrite(const void *buffer, size_t size, size_t count, FILE *fp) {
  if (!(fp->flag & FF_WRITE))
    return 0;

  return (fp->iof->write)(fp, buffer, size * count);
}
