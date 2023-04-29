#pragma once

#include <stddef.h>  // size_t
#include <stdint.h>  // intptr_t, uint64_t
#include <sys/types.h>  // ssize_t, pid_t, off_t

#define STDIN_FILENO   (0)
#define STDOUT_FILENO  (1)
#define STDERR_FILENO  (2)

struct clone_args {
  uint64_t flags;        /* Flags bit mask */
  uint64_t pidfd;        /* Where to store PID file descriptor
                            (int *) */
  uint64_t child_tid;    /* Where to store child TID,
                            in child's memory (pid_t *) */
  uint64_t parent_tid;   /* Where to store child TID,
                            in parent's memory (pid_t *) */
  uint64_t exit_signal;  /* Signal to deliver to parent on
                            child termination */
  uint64_t stack;        /* Pointer to lowest byte of stack */
  uint64_t stack_size;   /* Size of stack */
  uint64_t tls;          /* Location of new TLS */
  uint64_t set_tid;      /* Pointer to a pid_t array
                            (since Linux 5.5) */
  uint64_t set_tid_size; /* Number of elements in set_tid
                            (since Linux 5.5) */
  uint64_t cgroup;       /* File descriptor for target cgroup
                            of child (since Linux 5.7) */
};

ssize_t write(int fd, const void *str, size_t len);
int close(int fd);
ssize_t read(int fd, void *buf, size_t size);
off_t lseek(int fd, off_t offset, int whence);
int unlink(const char *pathname);
int rmdir(const char *pathname);
char *getcwd(char *buffer, size_t size);
int chdir(const char *path);

int brk(void *addr);
void *sbrk(intptr_t increment);

#if !defined(__wasm)
int unlinkat(int dirfd, const char *pathname, int flags);
int dup(int);
int pipe(int*);
int pipe2(int *pipefd, int flag);
int isatty(int fd);
ssize_t readlink(const char *pathname, char *buf, size_t bufsiz);
ssize_t readlinkat(int dirfd, const char *pathname, char *buf, size_t bufsiz);

pid_t fork(void);
long clone3(struct clone_args *cl_args, size_t size);
int execvp(const char *, char *const[]);
int execve(const char *, char *const[], char *const[]);
#endif
