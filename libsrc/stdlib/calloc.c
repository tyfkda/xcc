#include "stdlib.h"
#include "string.h"  // memset

void *calloc(size_t nmemb, size_t size) {
  size_t nbytes = nmemb * size;
  void *adr = malloc(nbytes);
  if (adr != NULL)
    memset(adr, 0, nbytes);
  return adr;
}
