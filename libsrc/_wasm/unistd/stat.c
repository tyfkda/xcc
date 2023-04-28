#include "sys/stat.h"
#include "string.h"  // memset, strncmp
#include "../wasi.h"

extern int max_preopen_fd;

void _set_stat(Filestat *fs, struct stat *st) {
  mode_t mode = 0;
  switch (fs->filetype) {
  case FILETYPE_BLOCK_DEVICE:  mode = S_IFBLK; break;
  case FILETYPE_CHARACTER_DEVICE:  mode = S_IFCHR; break;
  case FILETYPE_DIRECTORY:  mode = S_IFDIR; break;
  case FILETYPE_REGULAR_FILE:  mode = S_IFREG; break;
  case FILETYPE_SOCKET_DGRAM:  mode = S_IFSOCK; break;
  case FILETYPE_SOCKET_STREAM:  mode = S_IFSOCK; break;
  case FILETYPE_SYMBOLIC_LINK:  mode = S_IFLNK; break;
  }

  st->st_dev = fs->dev;
  st->st_ino = fs->ino;
  st->st_mode = mode;
  st->st_nlink = fs->nlink;
  st->st_uid = 0;
  st->st_gid = 0;
  st->st_rdev = 0;
  st->st_size = fs->size;
  st->st_blksize = 0;
  st->st_blocks = 0;
  // TODO: atim, mtim, ctim
}

int stat(const char *fn, struct stat *st) {
  memset(st, 0, sizeof(st));
  size_t fnlen = strlen(fn);
  for (int base_fd = 3; base_fd < max_preopen_fd; ++base_fd) {
    Prestat prestat;
    fd_prestat_get(base_fd, &prestat);
    size_t l = prestat.u.dir.pr_name_len;
    char buf[256];
    fd_prestat_dir_name(base_fd, buf, l);
    buf[l] = '\0';

    if ((*fn == '/' && *buf != '/') ||
        (*fn != '/' && (*buf != '.' || l != 1)))
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

    Filestat fs;
    uint32_t result = path_filestat_get(base_fd, 0, fn2, fnlen2, &fs);
    if (result == 0) {
      _set_stat(&fs, st);
      return 0;
    }
  }
  return 1;
}
