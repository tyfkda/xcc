#include "sys/stat.h"

int lstat(const char *fn, struct stat *st) {
  // TODO: Handle symbolic link?
  return stat(fn, st);
}
