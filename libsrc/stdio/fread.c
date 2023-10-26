#include "stdio.h"

#include "./_file.h"

size_t fread(void *buffer, size_t size, size_t count, FILE *fp) {
  if (!(fp->flag & FF_READ))
    return 0;
  ssize_t result = (fp->iof->read)(fp, buffer, size * count);
  return result >= 0 ? result / size : 0;
}
