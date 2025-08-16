#include "string.h"
#include "sys/types.h"  // ssize_t

ssize_t _canonicalize_path(char *buf, ssize_t size, const char *path);

ssize_t _canonicalize_path_cwd(char *buf, ssize_t size, const char *path) {
  extern char __cwd[];
  strncpy(buf, __cwd, size);
  return _canonicalize_path(buf, size, path);
}
