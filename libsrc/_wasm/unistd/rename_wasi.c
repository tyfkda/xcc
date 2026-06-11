#include "stdio.h"
#include "errno.h"
#include "libgen.h"  // dirname, basename
#include "limits.h"  // PATH_MAX
#include "stdlib.h"  // free
#include "string.h"
#include "../wasi.h"  // path_rename

#include "_search_preopen.h"

typedef struct {
  char paths[PATH_MAX];
  int base_fd;
} Info;

static bool search(int base_fd, const char *fn, size_t fnlen, void *data) {
  Info *info = data;
  if (fnlen > sizeof(info->paths))
    fnlen = sizeof(info->paths);
  strncpy(info->paths, fn, fnlen);
  info->base_fd = base_fd;
  return true;
}

int rename(const char *oldpath, const char *newpath) {
  char *newpath_copied = strdup(newpath);
  if (newpath_copied == NULL) {
    errno = -ENOMEM;
    return -1;
  }
  char *newdir = dirname(newpath_copied);

  int result = -1;
  Info oldinfo, newinfo;
  if (!_search_preopen(oldpath, &oldinfo, search) ||
      !_search_preopen(newdir, &newinfo, search)) {
    errno = -ENOENT;
  } else {
    char *newbn = basename((char*)newpath);
    size_t newbnlen = strlen(newbn);
    char newpath2[PATH_MAX];
    size_t len = strlen(newinfo.paths);
    if (len > 0) {
      strcpy(newpath2, newinfo.paths);
      newpath2[len] = '/';
      strcpy(&newpath2[len + 1], newbn);
      len += 1 + newbnlen;
    } else {
      strcpy(newpath2, newbn);
      len = newbnlen;
    }

    int r = path_rename(oldinfo.base_fd, oldinfo.paths, strlen(oldinfo.paths),
                        newinfo.base_fd, newpath2, len);
    if (r != 0) {
      errno = -EIO;
    } else {
      result = 0;
    }
  }
  free(newpath_copied);
  return result;
}
