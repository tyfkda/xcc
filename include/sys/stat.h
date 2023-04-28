#pragma once

typedef int mode_t;

#include <time.h>  // timespec
#include <stddef.h>  // size_t
#include <stdint.h>
#include <sys/types.h>  // off_t

typedef unsigned long dev_t;  // uint32_t, %ld
typedef unsigned long ino_t;  // uint64_t, %lu
typedef uint32_t uid_t;  // uint32_t, %d
typedef uint32_t gid_t;  // uint32_t, %d
typedef unsigned long blksize_t;  // uint32_t, %ld
typedef unsigned long blkcnt_t;  // uint64_t, %lu

#if defined(__x86_64__)
typedef unsigned long nlink_t;  // uint16_t, %ld

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

typedef unsigned int nlink_t;  // uint16_t, %ld

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

int chmod(const char *pathname, mode_t mode);
int fchmodat(int dirfd, const char *pathname, mode_t mode, int flags);

#define S_IFMT    0170000
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

int stat(const char *pathname, struct stat *buf);
int fstat(int fd, struct stat *buf);
int lstat(const char *pathname, struct stat *buf);
