#include "sys/stat.h"
#include "errno.h"
#include "string.h"  // memset, strncmp
#include "../wasi.h"

#include "_search_preopen.h"

static bool search_mkdir(int base_fd, const char *fn, size_t fnlen, void *data) {
  (void)data;
  uint32_t result = path_create_directory(base_fd, fn, fnlen);
  return result == 0;
}

int mkdir(const char *fn, mode_t mode) {
  (void)mode;
  if (_search_preopen(fn, NULL, search_mkdir))
    return 0;

  errno = EPERM;  // TODO
  return -1;
}
