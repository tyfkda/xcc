#include "stdio.h"
#include "stdlib.h"  // realloc

ssize_t getline(char **lineptr, size_t *pcapa, FILE *stream) {
  const int MIN_CAPA = 16;
  ssize_t capa = *pcapa;
  ssize_t size = 0;
  char *top = *lineptr;
  if (top == NULL || capa <= 0) {
    top = NULL;
    capa = 0;
  }
  for (;;) {
    int c = fgetc(stream);
    if (c == EOF) {
      if (size == 0)
        return -1;
      break;
    }

    if (size + 1 >= capa) {
      ssize_t newcapa = capa * 2;
      if (newcapa < MIN_CAPA)
        newcapa = MIN_CAPA;
      char *reallocated = realloc(top, newcapa);
      if (reallocated == NULL) {
        *lineptr = top;
        *pcapa = capa;
        return -1;
      }
      top = reallocated;
      capa = newcapa;
    }

    //assert(size < capa);
    top[size++] = c;

    if (c == '\n')
      break;
  }

  //assert(size < capa);
  top[size] = '\0';
  *lineptr = top;
  *pcapa = capa;
  return size;
}
