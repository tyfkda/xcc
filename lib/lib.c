#include "ctype.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"

#include "_file.h"
#include "sprintf.h"

#if defined(__XV6)
ssize_t write(int fd, const char *str, long len) {
  __asm("mov $16, %eax", 0xb8, 0x10, 0x00, 0x00, 0x00);  // SYS_write
  __asm("int $64",       0xcd, 0x40);
}

#elif defined(__linux__)
ssize_t write(int fd, const char *str, long len) {
#if defined(__XCC)
  __asm("mov $1, %eax", 0xb8, 0x01, 0x00, 0x00, 0x00);  // __NR_write
  __asm("syscall",      0x0f, 0x05);
#else
  __asm("mov $1, %eax\n"
        "syscall\n");
#endif
}

#else
#error Target not supported
#endif

int __assert_failed(const char *fn, int lineno) {
  fprintf(stderr, "Assert failed at %s(%d)\n", fn, lineno);
  exit(1);
}

int strlen(const char *s) {
  const char *p;
  for (p = s; *p != '\0'; ++p)
    ;
  return p - s;
}

char* strchr(const char *s, char c) {
  for (; *s != '\0'; ++s)
    if (*s == c)
      return (char*)s;
  return 0;
}

char* strrchr(const char *s, char c) {
  char* last = NULL;
  for(; *s != '\0'; ++s)
    if(*s == c)
      last = (char*)s;
  return last;
}

int strcmp(const char *p, const char *q) {
  while (*p != '\0' && *p == *q) {
    ++p;
    ++q;
  }
  return (int)*(unsigned char*)p - (int)*(unsigned char*)q;
}

int strncmp(const char *p, const char *q, size_t n) {
  while(n > 0 && *p == *q && *p != '\0')
    n--, p++, q++;
  return n == 0 ? 0 : (int)((unsigned char)*p - (unsigned char)*q);
}

char* strcpy(char *s, const char *t) {
  char *os = s;
  while((*s++ = *t++) != '\0')
    ;
  return os;
}

char* strncpy(char *s, const char *t, size_t n) {
  char *os = s;
  for (; n > 0 && (*s++ = *t++) != '\0'; --n)
    ;
  return os;
}

void* memcpy(void *dst, const void *src, size_t n) {
  const char *s = src;
  char *d = dst;
  while(n-- > 0)
    *d++ = *s++;
  return dst;
}

void* memset(void* buf, int val, size_t size) {
  unsigned char *p = buf;
  unsigned char v = val;
  for (size_t i = 0; i < size; ++i)
    *p++ = v;
  return buf;
}

long strtol(const char *p, char **pp, int base) {
  long result = 0;
  if (base <= 10) {
    for (;;) {
      char c = *p++;
      if ('0' <= c && c < ('0'+ base))
        result = result * base + (c - '0');
      else
        break;
    }
  } else {
    for (;;) {
      char c = *p++;
      if ('0' <= c && c <= '9')
        result = result * base + (c - '0');
      else if ('A' <= c && c < ('A' - 10 + base))
        result = result * base + (c - 'A' + 10);
      else if ('a' <= c && c < ('a' - 10 + base))
        result = result * base + (c - 'a' + 10);
      else
        break;
    }
  }

  if (pp != 0)
    *pp = (char*)(p - 1);

  return result;
}

static FILE _stdin = {0};
static FILE _stdout = {1};
static FILE _stderr = {2};
FILE *stdin = &_stdin;
FILE *stdout = &_stdout;
FILE *stderr = &_stderr;

size_t fwrite(const void* buffer, size_t size, size_t count, FILE* fp) {
  write(fp->fd, buffer, size * count);
}

size_t vfprintf(FILE *fp, const char *fmt, va_list ap) {
  // TODO: directly output to fd, not use vsnprintf.
  char buf[1024];
  size_t len = vsnprintf(buf, sizeof(buf), fmt, ap);
  return write(fp->fd, buf, len);
}

size_t fprintf(FILE *fp, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  size_t len = vfprintf(fp, fmt, ap);
  va_end(ap);
  return len;
}

size_t printf(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  size_t len = vfprintf(stdout, fmt, ap);
  va_end(ap);
  return len;
}

int atoi(const char* s) {
  int n = 0;
  for (; '0' <= *s && *s <= '9'; ++s)
    n = n * 10 + (*s - '0');
  return n;
}

//

int isdigit(int c) {
  return '0' <= c && c <= '9';
}

int isalpha(int c) {
  return ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z');
}

int isalnum(int c) {
  return isalpha(c) || isdigit(c);
}

int isspace(int c) {
  return (c == ' ' || c == '\t' || c == '\n' || c == '\r' ||
          c == '\f' || c == '\v');
}

#if defined(__XV6)

#elif defined(__linux__)
// fcntl.h
#define	O_RDONLY        0x0000          /* open for reading only */
#define	O_WRONLY        0x0001          /* open for writing only */
#define	O_RDWR          0x0002          /* open for reading and writing */
#define	O_ACCMODE       0x0003          /* mask for above modes */

#define	O_APPEND        0x0400          /* set append mode */
#define	O_CREAT         0x0040          /* create if nonexistant */
#define	O_TRUNC         0x0200          /* truncate to zero length */

//extern char **environ;
//char *environ[] = {"PATH=/bin:/usr/bin", NULL};
char **environ = NULL;

int open(const char *fn, int flag) {
#if defined(__XCC)
  __asm("mov $2, %eax", 0xb8, 0x02, 0x00, 0x00, 0x00);  // __NR_open
  __asm("syscall", 0x0f, 0x05);
#else
  __asm("mov $2, %eax\n"
        "syscall\n");
#endif
}

int close(int fd) {
#if defined(__XCC)
  __asm("mov $3, %eax", 0xb8, 0x03, 0x00, 0x00, 0x00);  // __NR_close
  __asm("syscall", 0x0f, 0x05);
#else
  __asm("mov $3, %eax\n"
        "syscall\n");
#endif
}

size_t read(int fd, void *buf, size_t size) {
#if defined(__XCC)
  __asm("mov $0, %eax", 0xb8, 0x00, 0x00, 0x00, 0x00);  // __NR_read
  __asm("syscall", 0x0f, 0x05);
#else
  __asm("mov $0, %eax\n"
        "syscall\n");
#endif
}

static size_t _getcwd(char *buffer, size_t size) {
#if defined(__XCC)
  __asm("mov $79, %eax", 0xb8, 0x4f, 0x00, 0x00, 0x00);  // __NR_getcwd
  __asm("syscall", 0x0f, 0x05);
#else
  __asm("mov $79, %eax\n"
        "syscall\n");
#endif
}

pid_t fork(void) {
#if defined(__XCC)
  __asm("mov $57, %eax", 0xb8, 0x39, 0x00, 0x00, 0x00);  // __NR_fork
  __asm("syscall", 0x0f, 0x05);
#else
  __asm("mov $57, %eax\n"
        "syscall\n");
#endif
}

int pipe(int *pipefd) {
#if defined(__XCC)
  __asm("mov $22, %eax", 0xb8, 0x16, 0x00, 0x00, 0x00);  // __NR_pipe
  __asm("syscall", 0x0f, 0x05);
#else
  __asm("mov $22, %eax\n"
        "syscall\n");
#endif
}

int dup(int fd) {
#if defined(__XCC)
  __asm("mov $32, %eax", 0xb8, 0x20, 0x00, 0x00, 0x00);  // __NR_dup
  __asm("syscall", 0x0f, 0x05);
#else
  __asm("mov $32, %eax\n"
        "syscall\n");
#endif
}

int execve(const char *path, char *const args[], char *const envp[]) {
#if defined(__XCC)
  __asm("mov $59, %eax", 0xb8, 0x3b, 0x00, 0x00, 0x00);  // __NR_execve
  __asm("syscall", 0x0f, 0x05);
#else
  __asm("mov $59, %eax\n"
        "syscall\n");
#endif
}

pid_t wait4(pid_t pid, int* status, int options, struct rusage *usage) {
#if defined(__XCC)
  __asm("mov $61, %eax", 0xb8, 0x3d, 0x00, 0x00, 0x00);  // __NR_wait4
  __asm("syscall", 0x0f, 0x05);
#else
  __asm("mov $61, %eax\n"
        "syscall\n");
#endif
}

int chmod(const char *pathname, /*mode_t*/int mode) {
#if defined(__XCC)
  __asm("mov $90, %eax", 0xb8, 0x5a, 0x00, 0x00, 0x00);  // __NR_chmod
  __asm("syscall", 0x0f, 0x05);
#else
  __asm("mov $90, %eax\n"
        "syscall\n");
#endif
}

off_t lseek(int fd, off_t offset, int whence) {
  __asm("mov $8, %eax", 0xb8, 0x08, 0x00, 0x00, 0x00);  // __NR_lseek
  __asm("syscall", 0x0f, 0x05);
}

#else
#error Target not supported
#endif

char *dirname(char *path) {
  char *p = strrchr(path, '/');
  if (p != NULL) {
    *p = '\0';
    return path;
  }

  fprintf(stderr, "dirname: not implemented\n");
  return NULL;
}

FILE* fopen(const char* fileName, const char* mode) {
  struct {
    const char* str;
    int flag;
  } static const kTable[] = {
    {"r", O_RDONLY},
    {"w", O_WRONLY | O_CREAT | O_TRUNC},
    {"a", O_WRONLY | O_CREAT | O_APPEND},
    {"rb", O_RDONLY},
    {"wb", O_WRONLY | O_CREAT | O_TRUNC},
    {"ab", O_WRONLY | O_CREAT | O_APPEND},
    {"r+", O_RDONLY},
    {"w+", O_WRONLY | O_CREAT},
    {"a+", O_WRONLY | O_CREAT | O_APPEND},
    {"r+b", O_RDONLY},
    {"w+b", O_WRONLY | O_CREAT},
    {"a+b", O_WRONLY | O_CREAT | O_APPEND},
    {"rb+", O_RDONLY},
    {"wb+", O_WRONLY | O_CREAT},
    {"ab+", O_WRONLY | O_CREAT | O_APPEND},
  };

  int flag = -1;
  for (int i = 0; i < sizeof(kTable) / sizeof(*kTable); ++i) {
    if (strcmp(kTable[i].str, mode) == 0) {
      flag = kTable[i].flag;
      break;
    }
  }
  if (flag == -1)
    return NULL;

  int fd = open(fileName, flag);
  if (fd < 0) {
    return NULL;
  }

  FILE* fp = malloc(sizeof(*fp));
  if (fp == NULL) {
    close(fd);
    return NULL;
  }

  fp->fd = fd;
  return fp;
}

int fclose(FILE* fp) {
  if (fp == NULL || fp->fd < 0)
    return EOF;
  close(fp->fd);
  fp->fd = -1;
  free(fp);
  return 0;
}

long ftell(FILE *fp) {
  return lseek(fp->fd, 0, SEEK_CUR);
}

int fgetc(FILE* fp) {
  unsigned char c;
  int len = read(fp->fd, &c, 1);
  return len == 1 ? (int)c : EOF;
}

int fputc(int c, FILE* fp) {
  unsigned char cc = c;
  int len = write(fp->fd, &cc, 1);
  return len == 1 ? c : EOF;
}

char *getcwd(char *buffer, size_t size) {
  if (buffer == NULL) {
    if (size == 0) {
      size = 512;  // PATH_MAX
    }
    buffer = malloc(size + 1);
    if (buffer == NULL)
      return NULL;
  }
  _getcwd(buffer, size);
  return buffer;
}

int execvp(const char *path, char *const args[]) {
  return execve(path, args, environ);
}

pid_t waitpid(int pid, int* status, int options) {
  return wait4(pid, status, options, NULL);
}

void perror(const char* msg) {
}
