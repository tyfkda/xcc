#include "unistd.h"
#include "errno.h"
#include "stdlib.h"  // malloc

#if defined(__WASM)
extern int _getcwd(char *, size_t);

#elif defined(__linux__)
static int _getcwd(char *buffer, size_t size) {
  __asm("mov $79, %eax\n"  // __NR_getcwd
        "syscall");
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
