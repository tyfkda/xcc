#pragma once

#include "stddef.h"  // size_t
#include "sys/types.h"  // ssize_t

#define STDIN_FILENO   (0)
#define STDOUT_FILENO  (1)
#define STDERR_FILENO  (2)

#define SEEK_SET  (0)
#define SEEK_CUR  (1)
#define SEEK_END  (2)

typedef int pid_t;
typedef long off_t;

void exit(int code);
ssize_t write(int fd, const char *str, size_t len);
int open(const char *fn, int flag);
int close(int fd);
ssize_t read(int fd, void *buf, size_t size);

char *getcwd(char *buffer, size_t size);

pid_t fork(void);
int pipe(int*);
int dup(int);
int execv(const char *, char *const[]);
int execvp(const char *, char *const[]);
int execve(const char*, char *const[], char *const[]);
off_t lseek(int fd, off_t offset, int whence);
