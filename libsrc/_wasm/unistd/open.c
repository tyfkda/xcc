#include "fcntl.h"
#include "errno.h"
#include "stdbool.h"
#include "../wasi.h"

#include "_search_preopen.h"

bool _search_preopen(const char *fn, void *data, SearchFileCallback callback);

typedef struct {
  int dirflags;
  int oflags;
  uint64_t fs_rights_base;
  uint32_t fdflags;
  uint64_t fs_rights_inheriting;
  uint32_t opened;
} OpenData;

static bool search_open(int base_fd, const char *fn, size_t fnlen, void *data) {
  OpenData *od = (OpenData*)data;
  uint32_t result = path_open(base_fd, od->dirflags, fn, fnlen, od->oflags, od->fs_rights_base,
                              od->fs_rights_inheriting, od->fdflags, &od->opened);
  return result == 0;
}

int open(const char *fn, int flag, ...) {
  OpenData od;
  od.dirflags = LOOKUPFLAGS_SYMLINK_FOLLOW;
  od.oflags = 0;
  od.fs_rights_base = 0;
  od.fdflags = 0; //FDFLAGS_RSYNC;
  switch (flag & O_ACCMODE) {
  case O_RDONLY:
    od.fs_rights_base =
        FD_READ | FD_SEEK | FD_FDSTAT_SET_FLAGS | FD_SYNC | FD_TELL | FD_ADVISE |
        PATH_CREATE_DIRECTORY | PATH_CREATE_FILE | PATH_LINK_SOURCE | PATH_LINK_TARGET |
        PATH_OPEN | FD_READDIR | PATH_READLINK | PATH_RENAME_SOURCE | PATH_RENAME_TARGET |
        PATH_FILESTAT_GET | FD_FILESTAT_GET | FD_FILESTAT_SET_TIMES | PATH_SYMLINK |
        PATH_REMOVE_DIRECTORY | PATH_UNLINK_FILE | POLL_FD_READWRITE;
    break;
  case O_WRONLY:
    od.fs_rights_base =
        FD_DATASYNC | FD_SEEK | FD_FDSTAT_SET_FLAGS | FD_SYNC | FD_TELL | FD_WRITE | FD_ADVISE |
        FD_ALLOCATE | PATH_CREATE_DIRECTORY | PATH_CREATE_FILE | PATH_LINK_SOURCE |
        PATH_LINK_TARGET | PATH_OPEN | FD_READDIR | PATH_READLINK | PATH_RENAME_SOURCE |
        PATH_RENAME_TARGET | PATH_FILESTAT_GET | FD_FILESTAT_GET | FD_FILESTAT_SET_SIZE |
        FD_FILESTAT_SET_TIMES | PATH_SYMLINK | PATH_REMOVE_DIRECTORY | PATH_UNLINK_FILE |
        POLL_FD_READWRITE;
    break;
  case O_RDWR:
    od.fs_rights_base =
        FD_DATASYNC | FD_READ | FD_SEEK | FD_FDSTAT_SET_FLAGS | FD_SYNC | FD_TELL | FD_WRITE |
        FD_ADVISE | FD_ALLOCATE | PATH_CREATE_DIRECTORY | PATH_CREATE_FILE | PATH_LINK_SOURCE |
        PATH_LINK_TARGET | PATH_OPEN | FD_READDIR | PATH_READLINK | PATH_RENAME_SOURCE |
        PATH_RENAME_TARGET | PATH_FILESTAT_GET | FD_FILESTAT_GET | FD_FILESTAT_SET_SIZE |
        FD_FILESTAT_SET_TIMES | PATH_SYMLINK | PATH_REMOVE_DIRECTORY | PATH_UNLINK_FILE |
        POLL_FD_READWRITE;
    break;
  default:
    errno = EINVAL;
    return -1;
  }
  od.fs_rights_inheriting = od.fs_rights_base;

  if (od.fs_rights_base & FD_WRITE) {
    if (flag & O_CREAT)
      od.oflags |= OFLAGS_CREAT;
    if (flag & O_TRUNC)
      od.oflags |= OFLAGS_TRUNC;
    if (flag & O_EXCL)
      od.oflags |= OFLAGS_EXCL;
  }

  if (_search_preopen(fn, &od, search_open))
    return od.opened;

  errno = ENOENT;  // (flag & O_CREAT) ? EEXIST, or something.
  return -1;
}
