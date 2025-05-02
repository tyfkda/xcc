#include "unistd.h"
#include "errno.h"
#include "fcntl.h"
#include "string.h"
#include "../wasi.h"

extern int __max_preopen_fd;

int open(const char *fn, int flag, ...) {
  int dirflags = LOOKUPFLAGS_SYMLINK_FOLLOW;
  int oflags = 0;
  uint64_t fs_rights_base = 0;
  uint32_t fdflags = 0; //FDFLAGS_RSYNC;
  switch (flag & O_ACCMODE) {
  case O_RDONLY:
    fs_rights_base = FD_READ | FD_SEEK | FD_FDSTAT_SET_FLAGS | FD_SYNC | FD_TELL | FD_ADVISE | PATH_CREATE_DIRECTORY | PATH_CREATE_FILE | PATH_LINK_SOURCE | PATH_LINK_TARGET | PATH_OPEN | FD_READDIR | PATH_READLINK | PATH_RENAME_SOURCE | PATH_RENAME_TARGET | PATH_FILESTAT_GET | FD_FILESTAT_GET | FD_FILESTAT_SET_TIMES | PATH_SYMLINK | PATH_REMOVE_DIRECTORY | PATH_UNLINK_FILE | POLL_FD_READWRITE;
    break;
  case O_WRONLY:
    fs_rights_base = FD_DATASYNC | FD_SEEK | FD_FDSTAT_SET_FLAGS | FD_SYNC | FD_TELL | FD_WRITE | FD_ADVISE | FD_ALLOCATE | PATH_CREATE_DIRECTORY | PATH_CREATE_FILE | PATH_LINK_SOURCE | PATH_LINK_TARGET | PATH_OPEN | FD_READDIR | PATH_READLINK | PATH_RENAME_SOURCE | PATH_RENAME_TARGET | PATH_FILESTAT_GET | FD_FILESTAT_GET | FD_FILESTAT_SET_SIZE | FD_FILESTAT_SET_TIMES | PATH_SYMLINK | PATH_REMOVE_DIRECTORY | PATH_UNLINK_FILE | POLL_FD_READWRITE;
    break;
  case O_RDWR:
    fs_rights_base = FD_DATASYNC | FD_READ | FD_SEEK | FD_FDSTAT_SET_FLAGS | FD_SYNC | FD_TELL | FD_WRITE | FD_ADVISE | FD_ALLOCATE | PATH_CREATE_DIRECTORY | PATH_CREATE_FILE | PATH_LINK_SOURCE | PATH_LINK_TARGET | PATH_OPEN | FD_READDIR | PATH_READLINK | PATH_RENAME_SOURCE | PATH_RENAME_TARGET | PATH_FILESTAT_GET | FD_FILESTAT_GET | FD_FILESTAT_SET_SIZE | FD_FILESTAT_SET_TIMES | PATH_SYMLINK | PATH_REMOVE_DIRECTORY | PATH_UNLINK_FILE | POLL_FD_READWRITE;
    break;
  default:
    errno = EINVAL;
    return -1;
  }
  uint64_t fs_rights_inheriting = fs_rights_base;

  if (fs_rights_base & FD_WRITE) {
    if (flag & O_CREAT)
      oflags |= OFLAGS_CREAT;
    if (flag & O_TRUNC)
      oflags |= OFLAGS_TRUNC;
    if (flag & O_EXCL)
      oflags |= OFLAGS_EXCL;
  }

  // Search from preopens
  size_t fnlen = strlen(fn);
  for (int base_fd = 3; base_fd < __max_preopen_fd; ++base_fd) {
    Prestat prestat;
    fd_prestat_get(base_fd, &prestat);
    size_t l = prestat.u.dir.pr_name_len;  // Includes '\0' or not, depending on the environment,
    char buf[256];
    fd_prestat_dir_name(base_fd, buf, l);
    buf[l] = '\0';

    if ((*fn == '/' && *buf != '/') ||
        (*fn != '/' && strcmp(buf, ".") != 0))
      continue;

    const char *fn2 = fn;
    size_t fnlen2 = fnlen;

    if (strncmp(fn, buf, l) == 0 && fn[l] == '/') {
      fn2 = fn + (l + 1);
      fnlen2 = fnlen - (l + 1);
    } else if (l == 1 && *buf == '/' && *fn == '/') {
      fn2 = fn + 1;
      fnlen2 = fnlen - 1;
    }
    uint32_t opened = -1;
    uint32_t result = path_open(base_fd, dirflags, fn2, fnlen2, oflags, fs_rights_base,
                                fs_rights_inheriting, fdflags, &opened);
    if (result == 0)
      return opened;
  }
  errno = ENOENT;  // (flag & O_CREAT) ? EEXIST, or something.
  return -1;
}
