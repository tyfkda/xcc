#pragma once

#include <stddef.h>  // size_t

int args_sizes_get(int *pargc, int *plen);
int args_get(char **pargv, char *pstr);
void proc_exit(int);

int fd_write(int fd, const void *iov, int count, size_t *out);

int random_get(void *buf, size_t buf_len);
