#include "sys/stat.h"
#include "../wasi.h"

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
