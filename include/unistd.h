#pragma once

#include "stddef.h"  // size_t
#include "stdint.h"  // intptr_t
#include "sys/types.h"  // ssize_t, pid_t, off_t

#define STDIN_FILENO   (0)
#define STDOUT_FILENO  (1)
#define STDERR_FILENO  (2)

ssize_t write(int fd, const void *str, size_t len);
int close(int fd);
ssize_t read(int fd, void *buf, size_t size);

char *getcwd(char *buffer, size_t size);

pid_t fork(void);
int pipe(int *);
int dup(int);
int execvp(const char *, char *const[]);
int execve(const char *, char *const[], char *const[]);
off_t lseek(int fd, off_t offset, int whence);
int unlink(const char *pathname);

int brk(void *addr);
void *sbrk(intptr_t increment);

int isatty(int fd);
