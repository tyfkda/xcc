#include "unistd.h"

#include "errno.h"
#include "limits.h"
#include "string.h"
#include "sys/stat.h"

extern ssize_t _canonicalize_path_cwd(char *buf, ssize_t size, const char *path);
extern char __cwd[];

int chdir(const char *path) {
  char buf[PATH_MAX];
  ssize_t p = _canonicalize_path_cwd(buf, PATH_MAX, path);
  if (p < 0) {
    errno = ENOENT;
    return -1;
  }
  if (p > 0 && buf[p - 1] == '/')  // Remove trailing slash.
    buf[--p] = '\0';

  struct stat st;
  int r = stat(buf, &st);
  if (r != 0)
    return r;
  if (!S_ISDIR(st.st_mode)) {
    errno = ENOTDIR;
    return -1;
  }

  strncpy(__cwd, buf, PATH_MAX);
  return 0;
}
