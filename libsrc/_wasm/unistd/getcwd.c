#include "unistd.h"
#include "errno.h"
#include "limits.h"
#include "stdlib.h"  // malloc
#include "string.h"  // strlen

char __cwd[PATH_MAX] = ".";

#define MIN(a, b)  ((a) < (b) ? (a) : (b))

static inline void GETCWD(char *dst, size_t siz) {
  strncpy(dst, __cwd, siz);
}

char *getcwd(char *buffer, size_t size) {
  if (buffer == NULL) {
    if (size == 0)
      size = PATH_MAX;
    buffer = malloc(size);
    if (buffer == NULL)
      return NULL;
  }
  GETCWD(buffer, size);
  return buffer;
}
