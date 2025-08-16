#include "sys/stat.h"
#include "errno.h"
#include "string.h"  // memset, strncmp
#include "../wasi.h"

#include "_search_preopen.h"

extern void _set_stat(Filestat *fs, struct stat *st);

static bool search_stat(int base_fd, const char *fn, size_t fnlen, void *data) {
  struct stat *st = data;
  Filestat fs;
  uint32_t result = path_filestat_get(base_fd, 0, fn, fnlen, &fs);
  if (result != 0)
    return false;
  _set_stat(&fs, st);
  return true;
}

int stat(const char *fn, struct stat *st) {
  memset(st, 0, sizeof(*st));

  if (_search_preopen(fn, st, search_stat))
    return 0;

  errno = ENOENT;
  return -1;
}
