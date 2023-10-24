#include "stdio.h"

#include "./_file.h"

size_t fread(void *buffer, size_t size, size_t count, FILE *fp) {
  if (!(fp->flag & FF_READ))
    return 0;
  return (fp->iof->read)(fp, buffer, size * count);
}
