#pragma once

#include <stddef.h>  // size_t
#include <stdint.h>

extern int max_preopen_fd;

// Fdflags
#define FDFLAGS_APPEND           (1 << 0)
#define FDFLAGS_DSYNC            (1 << 1)
#define FDFLAGS_NONBLOCK         (1 << 2)
#define FDFLAGS_RSYNC            (1 << 3)
#define FDFLAGS_SYNC             (1 << 4)

// Lookupflags
#define LOOKUPFLAGS_SYMLINK_FOLLOW  (1 << 0)

#define OFLAGS_CREAT      (1 << 0)
#define OFLAGS_DIRECTORY  (1 << 1)
#define OFLAGS_EXCL       (1 << 2)
#define OFLAGS_TRUNC      (1 << 3)

// Rights
#define FD_DATASYNC              (1UL << 0)
#define FD_READ                  (1UL << 1)
#define FD_SEEK                  (1UL << 2)
#define FD_FDSTAT_SET_FLAGS      (1UL << 3)
#define FD_SYNC                  (1UL << 4)
#define FD_TELL                  (1UL << 5)
#define FD_WRITE                 (1UL << 6)
#define FD_ADVISE                (1UL << 7)
#define FD_ALLOCATE              (1UL << 8)
#define PATH_CREATE_DIRECTORY    (1UL << 9)
#define PATH_CREATE_FILE         (1UL << 10)
#define PATH_LINK_SOURCE         (1UL << 11)
#define PATH_LINK_TARGET         (1UL << 12)
#define PATH_OPEN                (1UL << 13)
#define FD_READDIR               (1UL << 14)
#define PATH_READLINK            (1UL << 15)
#define PATH_RENAME_SOURCE       (1UL << 16)
#define PATH_RENAME_TARGET       (1UL << 17)
#define PATH_FILESTAT_GET        (1UL << 18)
#define PATH_FILESTAT_SET_SIZE   (1UL << 19)
#define PATH_FILESTAT_SET_TIMES  (1UL << 20)
#define FD_FILESTAT_GET          (1UL << 21)
#define FD_FILESTAT_SET_SIZE     (1UL << 22)
#define FD_FILESTAT_SET_TIMES    (1UL << 23)
#define PATH_SYMLINK             (1UL << 24)
#define PATH_REMOVE_DIRECTORY    (1UL << 25)
#define PATH_UNLINK_FILE         (1UL << 26)
#define POLL_FD_READWRITE        (1UL << 27)
#define SOCK_SHUTDOWN            (1UL << 28)
#define SOCK_ACCEPT              (1UL << 29)

typedef struct {
  const char *str;
  size_t n;
} Iov;

typedef uint8_t Preopentype;

typedef struct {
  /// The length of the directory name for use with `fd_prestat_dir_name`.
  size_t pr_name_len;
} PrestatDir;

typedef struct {
  PrestatDir dir;
} PrestatU;

typedef struct {
  /// The type of the pre-opened capability.
  Preopentype pr_type;
  /// The contents of the information.
  PrestatU u;
} Prestat;

int fd_prestat_get(int fd, Prestat *prestat);
int fd_prestat_dir_name(int fd, char *out, size_t size);
