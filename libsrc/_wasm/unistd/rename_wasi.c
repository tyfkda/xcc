#include "unistd.h"
#include "errno.h"
#include "libgen.h"  // dirname, basename
#include "limits.h"  // PATH_MAX
#include "stdlib.h"  // malloc, free
#include "string.h"
#include "../wasi.h"

#include "_search_preopen.h"

#include "stdio.h"

typedef struct {
  char paths[2][PATH_MAX];
  int base_fd[2];
  int phase;
} Info;

static bool search(int base_fd, const char *fn, size_t fnlen, void *data) {
  Info *info = data;
  int phase = info->phase;
fprintf(stderr, "rename: search, base_fd=%d, fn=[%.*s]\n", base_fd, (int)fnlen, fn);
  if (fnlen > sizeof(*info->paths[0]))
    fnlen = sizeof(*info->paths[0]);
  strncpy(info->paths[phase], fn, fnlen);
  info->base_fd[phase] = base_fd;
  return true;
}

int rename(const char *oldpath, const char *newpath) {
  char *newpath_copied = strdup(newpath);
  if (newpath_copied == NULL) {
    errno = -ENOMEM;
    return -1;
  }
  char *newdir = dirname(newpath_copied);
fprintf(stderr, "rename: newpath=%s, newdir=[%s]\n", newpath, newdir);

  int result = -1;
  Info info;
  info.phase = 0;
  if (!_search_preopen(oldpath, &info, search)) {
    errno = -ENOENT;
  } else {
    info.phase = 1;
    if (!_search_preopen(newdir, &info, search)) {
      errno = -ENOENT;
    } else {
      // size_t newdirlen = strlen(newdir);
      char *newbn = basename((char*)newpath);
      size_t newbnlen = strlen(newbn);
      // char *newpath2 = malloc(newdirlen + newbnlen + 2);
      // if (newpath2 == NULL) {
      //   errno = -ENOMEM;
      // } else {
      //   strcpy(newpath2, newdir);
      //   newpath2[newdirlen] = '/';
      //   strcpy(&newpath2[newdirlen + 1], newbn);
fprintf(stderr, "rename: newbn=%s\n", newbn);

        char newpath2[PATH_MAX];
        size_t len = strlen(info.paths[1]);
        if (len > 0) {
          strcpy(newpath2, info.paths[1]);
          newpath2[len] = '/';
          strcpy(&newpath2[len + 1], newbn);
          len += 1 + newbnlen;
        } else {
          strcpy(newpath2, newbn);
          len = newbnlen;
        }
fprintf(stderr, "rename: newpath2=[%s], %zu\n", newpath2, len);

        int r = path_rename(info.base_fd[0], info.paths[0], strlen(info.paths[0]), info.base_fd[1], newpath2, len);
        if (r != 0) {
          errno = -EIO;
        } else {
          result = 0;
        }
      //   free(newpath2);
      // }
    }
  }
  free(newpath_copied);
  return result;
}
