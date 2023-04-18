#include "unistd.h"
#include "errno.h"
#include "stdlib.h"  // malloc

extern int _getcwd(char *, size_t);

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
  int result = _getcwd(buffer, size);
  if (result < 0) {
    errno = -result;
    free(allocated);
    return NULL;
  }
  return buffer;
}
