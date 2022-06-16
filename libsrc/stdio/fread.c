#include "stdio.h"
#include "unistd.h"  // read

#include "./_file.h"

size_t fread(void *buffer, size_t size, size_t count, FILE *fp) {
  ssize_t readed = read(fp->fd, buffer, size * count);
  if (readed == -1)
    return 0;
  return (size_t)readed / size;
}
