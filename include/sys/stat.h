#pragma once

#include <time.h>  // timespec
#include <stddef.h>  // size_t
#include <stdint.h>
#include <sys/types.h>  // off_t

typedef unsigned long ino_t;
typedef uint32_t uid_t;
typedef uint32_t gid_t;
typedef unsigned long blkcnt_t;

#if defined(__APPLE__)
typedef int32_t dev_t;
typedef uint16_t mode_t;
typedef uint16_t nlink_t;
typedef int32_t blksize_t;

// __DARWIN_STRUCT_STAT64
struct stat {
  dev_t       st_dev;                 /* [XSI] ID of device containing file */
  mode_t      st_mode;                /* [XSI] Mode of file (see below) */
  nlink_t     st_nlink;               /* [XSI] Number of hard links */
  uint64_t    st_ino;                 /* [XSI] File serial number */
  uid_t       st_uid;                 /* [XSI] User ID of the file */
  gid_t       st_gid;                 /* [XSI] Group ID of the file */
  dev_t       st_rdev;                /* [XSI] Device ID */

  // __DARWIN_STRUCT_STAT64_TIMES
  struct timespec st_atimespec;           /* time of last access */
  struct timespec st_mtimespec;           /* time of last data modification */
  struct timespec st_ctimespec;           /* time of last status change */
  struct timespec st_birthtimespec;       /* time of file creation(birth) */

  off_t       st_size;                /* [XSI] file size, in bytes */
  blkcnt_t    st_blocks;              /* [XSI] blocks allocated for file */
  blksize_t   st_blksize;             /* [XSI] optimal blocksize for I/O */
  uint32_t    st_flags;               /* user defined flags for file */
  uint32_t    st_gen;                 /* file generation number */
  int32_t     st_lspare;              /* RESERVED: DO NOT USE! */
  int64_t     st_qspare[2];           /* RESERVED: DO NOT USE! */
};
#elif defined(__x86_64__)
typedef unsigned long dev_t;
typedef int mode_t;
typedef unsigned long nlink_t;
typedef unsigned long blksize_t;

struct stat {
  dev_t     st_dev;
  ino_t     st_ino;
  nlink_t   st_nlink;
  mode_t    st_mode;
  uid_t     st_uid;
  gid_t     st_gid;
  int __pad0;
  dev_t     st_rdev;
  off_t     st_size;
  blksize_t st_blksize;
  blkcnt_t  st_blocks;

  struct timespec st_atim;
  struct timespec st_mtim;
  struct timespec st_ctim;

#define st_atime st_atim.tv_sec
#define st_mtime st_mtim.tv_sec
#define st_ctime st_ctim.tv_sec

  /*__syscall_slong_t*/ long __glibc_reserved[3];
};

#else
typedef unsigned long dev_t;
typedef int mode_t;
typedef unsigned int nlink_t;
typedef unsigned long blksize_t;

struct stat {
  dev_t     st_dev;
  ino_t     st_ino;
  mode_t    st_mode;
  nlink_t   st_nlink;
  uid_t     st_uid;
  gid_t     st_gid;
  dev_t     st_rdev;
  dev_t __pad1;
  off_t     st_size;
  blksize_t st_blksize;
  blkcnt_t  st_blocks;

  struct timespec st_atim;
  struct timespec st_mtim;
  struct timespec st_ctim;

#define st_atime st_atim.tv_sec
#define st_mtime st_mtim.tv_sec
#define st_ctime st_ctim.tv_sec

  int __glibc_reserved[2];
};
#endif

int mkdir(const char *pathname, mode_t mode);
int mkdirat(int dirfd, const char *pathname, mode_t mode);

int chmod(const char *pathname, mode_t mode);
int fchmod(int fd, mode_t mode);
int fchmodat(int dirfd, const char *pathname, mode_t mode, int flags);

#define S_IFMT    0170000
#define S_IFIFO   0010000
#define S_IFCHR   0020000
#define S_IFDIR   0040000
#define S_IFBLK   0060000
#define S_IFREG   0100000
#define S_IFLNK   0120000
#define S_IFSOCK  0140000

#define S_ISREG(m)   (((m) & S_IFMT) == S_IFREG)
#define S_ISDIR(m)   (((m) & S_IFMT) == S_IFDIR)
#define S_ISCHR(m)   (((m) & S_IFMT) == S_IFCHR)
#define S_ISBLK(m)   (((m) & S_IFMT) == S_IFBLK)
#define S_ISLNK(m)   (((m) & S_IFMT) == S_IFLNK)
#define S_ISSOCK(m)  (((m) & S_IFMT) == S_IFSOCK)
#define S_ISFIFO(m)  (((m) & S_IFMT) == S_IFFIFO)

int stat(const char *pathname, struct stat *buf);
int fstat(int fd, struct stat *buf);
int lstat(const char *pathname, struct stat *buf);
int fstatat(int fd, const char *pathname, struct stat *buf, int flag);

#if defined(__APPLE__)
// Patch for Rosetta2/x64
int stat64(const char *pathname, struct stat *buf);
int fstat64(int fd, struct stat *buf);
int lstat64(const char *pathname, struct stat *buf);
int fstatat64(int fd, const char *pathname, struct stat *buf, int flag);
#define stat(pathname, buf)   stat64(pathname, buf)
#define fstat(pathname, buf)  fstat64(pathname, buf)
#define lstat(pathname, buf)  lstat64(pathname, buf)
#define fstatat(fd, p, b, f)  fstatat64(fd, p, b, f)
#endif
