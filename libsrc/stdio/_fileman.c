#include "stdio.h"

#include "stdlib.h"  // realloc

#include "_file.h"
#include "_fileman.h"

#define INITIAL_CAPACITY  (4)

FILEMAN __fileman;

void _add_opened_file(FILE *fp) {
  if (__fileman.length >= __fileman.capacity) {
    int ncapa = __fileman.capacity > 0 ? __fileman.capacity << 1 : INITIAL_CAPACITY;
    FILE **buf = realloc(__fileman.opened, ncapa * sizeof(*__fileman.opened));
    if (buf == NULL) {
      // TODO:
      fputs("Out of memory\n", stderr);
      exit(1);
    }
    __fileman.opened = buf;
    __fileman.capacity = ncapa;
  }
  __fileman.opened[__fileman.length++] = fp;
}

void _remove_opened_file(FILE *fp) {
  FILE **files = __fileman.opened;
  for (int i = 0, length = __fileman.length; i < length; ++i) {
    if (files[i] == fp) {
      --length;
      if (i < length) {
        // Swap to the last.
        files[i] = files[length];
      }
      __fileman.length = length;
      break;
    }
  }
}
