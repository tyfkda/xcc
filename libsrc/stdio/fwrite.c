#include "stdio.h"
#include "unistd.h"  // write

#include "./_file.h"

size_t fwrite(const void *buffer, size_t size, size_t count, FILE *fp) {
  if (!(fp->flag & FF_WRITE))
    return 0;

  ssize_t result = (fp->iof->write)(fp, buffer, size * count);
  return result >= 0 ? result / size : 0;
}
