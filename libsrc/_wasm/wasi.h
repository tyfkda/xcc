#pragma once

#include <stddef.h>  // size_t
#include <stdint.h>

int args_sizes_get(int *pargc, int *plen);
int args_get(char **pargv, char *pstr);
void proc_exit(int);

int path_open(int fd, int dirflags, const char *path, int path_len, int oflags,
              uint64_t fs_rights_base, uint64_t fs_rights_inherting, uint16_t fdflags,
              uint32_t *opend_fd);
int path_unlink_file(int fd, const char *path, int path_len);

int fd_read(int fd, const void *iov, int count, size_t *out);
int fd_write(int fd, const void *iov, int count, size_t *out);
int fd_close(int fd);
int fd_seek(int fd, int64_t offset, int whence, size_t *psize);

int clock_time_get(int clockid, uint64_t precision, uint64_t *out);

int random_get(void *buf, size_t buf_len);
