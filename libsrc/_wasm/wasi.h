#pragma once

#include <stddef.h>  // size_t
#include <stdint.h>

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

typedef uint64_t Timestamp;

typedef struct Filestat {
  uint64_t dev;
  uint64_t ino;
  uint8_t filetype;
  uint64_t nlink;
  uint64_t size;
  Timestamp atim;
  Timestamp mtim;
  Timestamp ctim;
} Filestat;

enum Filetype {
  FILETYPE_UNKNOWN,
  FILETYPE_BLOCK_DEVICE,
  FILETYPE_CHARACTER_DEVICE,
  FILETYPE_DIRECTORY,
  FILETYPE_REGULAR_FILE,
  FILETYPE_SOCKET_DGRAM,
  FILETYPE_SOCKET_STREAM,
  FILETYPE_SYMBOLIC_LINK,
};

int args_sizes_get(int *pargc, int *plen);
int args_get(char **pargv, char *pstr);
void proc_exit(int);

int fd_prestat_get(int fd, Prestat *prestat);
int fd_prestat_dir_name(int fd, char *out, size_t size);

int path_open(int fd, int dirflags, const char *path, size_t path_len, int oflags,
              uint64_t fs_rights_base, uint64_t fs_rights_inherting, uint16_t fdflags,
              uint32_t *opend_fd);
int path_unlink_file(int fd, const char *path, size_t path_len);
int path_filestat_get(int fd, int flags, const char *path, size_t path_len, Filestat *out);

int fd_read(int fd, const void *iov, int count, size_t *out);
int fd_write(int fd, const void *iov, int count, size_t *out);
int fd_close(int fd);
int fd_seek(int fd, int64_t offset, int whence, size_t *psize);
int fd_filestat_get(int fd, Filestat *out);

int clock_time_get(int clockid, uint64_t precision, uint64_t *out);

int random_get(void *buf, size_t buf_len);
