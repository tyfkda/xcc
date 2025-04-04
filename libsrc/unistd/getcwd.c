#include "unistd.h"
#include "errno.h"
#include "stdlib.h"  // malloc

#if defined(__APPLE__)
extern int _getcwd(char *, size_t);

#elif defined(__linux__)
#include "_syscall.h"

static int _getcwd(char *buffer, size_t size) {
  int ret;
  SYSCALL_RET(__NR_getcwd, ret);
  SET_ERRNO(ret);
  return ret;
}
#endif

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
