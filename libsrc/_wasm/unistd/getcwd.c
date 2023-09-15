#include "unistd.h"
#include "errno.h"
#include "stdlib.h"  // malloc
#include "string.h"  // strlen

const char *__cwd = ".";

#define MIN(a, b)  ((a) < (b) ? (a) : (b))
#define GETCWD(dst, siz)  ({size_t _s = strlen(__cwd); size_t _m = MIN(_s + 1, siz); strncpy(dst, __cwd, _m); _m; })

char *getcwd(char *buffer, size_t size) {
  void *allocated = NULL;
  if (buffer == NULL) {
    if (size == 0) {
      size = 512;  // PATH_MAX
    }
    buffer = allocated = malloc(size + 1);
    if (buffer == NULL)
      return NULL;
  }
  int result = GETCWD(buffer, size);
  if (result < 0) {
    errno = -result;
    free(allocated);
    return NULL;
  }
  return buffer;
}
