#include "stdio.h"
#include "unistd.h"  // write

#include "./_file.h"

size_t fwrite(const void *buffer, size_t size, size_t count, FILE *fp) {
  return write(fp->fd, buffer, size * count);
}
