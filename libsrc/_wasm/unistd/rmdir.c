#include "unistd.h"
#include "errno.h"
#include "string.h"
#include "../wasi.h"

#include "_search_preopen.h"

static bool search_rmdir(int base_fd, const char *fn, size_t fnlen, void *data) {
  (void)data;
  int result = path_remove_directory(base_fd, fn, fnlen);
  return result == 0;
}

int rmdir(const char *fn) {
  if (_search_preopen(fn, NULL, search_rmdir))
    return 0;

  errno = ENOENT;
  return -1;
}
