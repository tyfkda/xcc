#include "unistd.h"
#include "errno.h"
#include "string.h"
#include "../wasi.h"

#include "_search_preopen.h"

static bool search_unlink(int base_fd, const char *fn, size_t fnlen, void *data) {
  (void)data;
  int result = path_unlink_file(base_fd, fn, fnlen);
  return result == 0;
}

int unlink(const char *fn) {
  if (_search_preopen(fn, NULL, search_unlink))
    return 0;

  errno = ENOENT;  // TODO
  return -1;
}
