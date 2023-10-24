#include "stdio.h"
#include "unistd.h"  // write

#include "./_file.h"

size_t fwrite(const void *buffer, size_t size, size_t count, FILE *fp) {
  if (!(fp->flag & FF_WRITE))
    return 0;

  int (*fputc)(int, FILE*) = fp->fputc;
  const unsigned char *src = (const unsigned char*)buffer;
  size_t total = size * count, i;
  for (i = 0; i < total; ++i) {
    if (fputc(*src, fp) == EOF)
      break;
    ++src;
  }
  return i;
}
