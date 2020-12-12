#include "ctype.h"
#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"

#include "_file.h"

#if defined(__XV6)
ssize_t write(int fd, const void *str, size_t len) {
  __asm("mov $16, %eax");  // SYS_write
  __asm("int $64");
}

#elif defined(__linux__)
ssize_t write(int fd, const void *str, size_t len) {
#if defined(__XCC)
  __asm("mov $1, %eax");  // __NR_write
  __asm("syscall\n");
#else
  ssize_t ret;
  __asm("mov $1, %%eax\n\t"  // __NR_write
        "syscall"
        : "=r"(ret));
  return ret;
#endif
}

#elif defined(__APPLE__)

// Use libc.
#define USE_LIBC

#else
#error Target not supported
#endif

#ifndef USE_LIBC

size_t strlen(const char *s) {
  const char *p;
  for (p = s; *p != '\0'; ++p)
    ;
  return p - s;
}

char *strchr(const char *s, int c) {
  for (; *s != '\0'; ++s)
    if (*s == c)
      return (char*)s;
  return 0;
}

char *strrchr(const char *s, int c) {
  char *last = NULL;
  for(; *s != '\0'; ++s)
    if(*s == c)
      last = (char*)s;
  return last;
}

char *strstr(const char *s1, const char *s2) {
  for  (size_t len = strlen(s2); *s1 != '\0'; ++s1) {
    if (strncmp(s1, s2, len) == 0)
      return s1;
  }
  return NULL;
}

int strcmp(const char *p, const char *q) {
  while (*p != '\0' && *p == *q) {
    ++p;
    ++q;
  }
  return (int)*(unsigned char*)p - (int)*(unsigned char*)q;
}

int strncmp(const char *p, const char *q, size_t n) {
  while (n > 0 && *p == *q && *p != '\0')
    n--, p++, q++;
  return n == 0 ? 0 : (unsigned char)*p - (unsigned char)*q;
}

int strcasecmp(const char *p, const char *q) {
  for (;; ++p, ++q) {
    unsigned char c1 = *p;
    unsigned char c2 = *q;
    int d = c1 - c2;
    if (d != 0)
      return d;
    if (c1 == 0)
      break;
  }
  return 0;
}

int strncasecmp(const char *p, const char *q, size_t n) {
  for (; n > 0; --n, ++p, ++q) {
    int c1 = tolower((unsigned char)*p);
    int c2 = tolower((unsigned char)*q);
    int d = c1 - c2;
    if (d != 0)
      return d;
    if (c1 == 0)
      break;
  }
  return 0;
}

char *strcpy(char *s, const char *t) {
  char *os = s;
  while ((*s++ = *t++) != '\0')
    ;
  return os;
}

char *strncpy(char *s, const char *t, size_t n) {
  char *os = s;
  for (; n > 0 && (*s++ = *t++) != '\0'; --n)
    ;
  return os;
}

void *memcpy(void *dst, const void *src, size_t n) {
  const char *s = src;
  char *d = dst;
  while (n-- > 0)
    *d++ = *s++;
  return dst;
}

void *memmove(void *dst, const void *src, size_t n) {
  const char *s = src;
  char *d = dst;
  if (s < d && s + n > d) {
    s += n;
    d += n;
    while (n-- > 0)
      *--d = *--s;
  } else {
    while (n-- > 0)
      *d++ = *s++;
  }
  return dst;
}

void *memset(void *buf, int val, size_t size) {
  unsigned char *p = buf;
  unsigned char v = val;
  for (size_t i = 0; i < size; ++i)
    *p++ = v;
  return buf;
}

int memcmp(const void *buf1, const void *buf2, size_t n) {
  const unsigned char *p = buf1;
  const unsigned char *q = buf2;
  int d;
  for (size_t i = 0; i < n; ++i, ++p, ++q) {
    d = (int)*(unsigned char*)p - (int)*(unsigned char*)q;
    if (d != 0)
      break;
  }
  return d;
}

static bool parse_sign(const char **pp) {
  const char *p = *pp;
  char c = *p;
  bool negative = c == '-';
  if (c == '+' || c == '-')
    *pp = p + 1;
  return negative;
}

long strtol(const char *p, char **pp, int base) {
  const char *orig = p;
  bool neg = parse_sign(&p);
  char digimax = '0' + (base <= 10 ? base : 10);
  char hexmax = 'a' - 10 + base;
  long result = 0;
  const char *op = p;
  for (;; ++p) {
    char c = *p;
    int n;
    if ('0' <= c && c < digimax)
      n = c - '0';
    else {
      c = tolower(c);
      if ('a' <= c && c < hexmax)
        n = c - 'a' + 10;
      else
        break;
    }
    result = result * base + n;
  }
  if (p == op)
    p = orig;
  if (neg)
    result = -result;

  if (pp != 0)
    *pp = (char*)p;

  return result;
}

unsigned long strtoul(const char *p, char **pp, int base) {
  const char *orig = p;
  if (*p == '+')
    ++p;
  char digimax = '0' + (base <= 10 ? base : 10);
  char hexmax = 'a' - 10 + base;
  unsigned long result = 0;
  const char *op = p;
  for (;; ++p) {
    char c = *p;
    int n;
    if ('0' <= c && c < digimax)
      n = c - '0';
    else {
      c = tolower(c);
      if ('a' <= c && c < hexmax)
        n = c - 'a' + 10;
      else
        break;
    }
    result = result * base + n;
  }
  if (p == op)
    p = orig;

  if (pp != 0)
    *pp = (char*)p;

  return result;
}

#ifndef __NO_FLONUM
static double ipow(double base, long x) {
  bool neg = false;
  if (x < 0) {
    neg = true;
    x = -x;
  }
  double result = 1;
  double a = base;
  for (; x > 0; x >>= 1, a *= a) {
    if ((x & 1) != 0)
      result *= a;
  }
  return neg ? 1.0 / result : result;
}

static double strtod_i(const char *p, const char **pp) {
  double result = 0;
  for (;; ++p) {
    char c = *p;
    if (!(c >= '0' && c <= '9'))
      break;
    result = result * 10 + (c - '0');
  }
  *pp = p;
  return result;
}

double strtod(const char* /*restrict*/ p, char ** /*restrict*/ pp) {
  const char *orig = p;
  bool neg = parse_sign(&p);

  struct {
    const char *str;
    double pos, neg;
  } static const CONST[] = {
    {"infinity", 1.0 / 0.0, -1.0 / 0.0},
    {"inf", 1.0 / 0.0, -1.0 / 0.0},
    {"nan", 0.0 / 0.0, 0.0 / 0.0},
  };
  for (int i = 0, n = sizeof(CONST) / sizeof(*CONST); i < n; ++i) {
    const char *str = CONST[i].str;
    size_t len = strlen(str);
    if (strncmp(p, str, len) == 0) {
      p += len;
      char c = *p;
      if (pp != 0)
        *pp = (char*)p;
      return neg ? CONST[i].neg : CONST[i].pos;
    }
  }

  const char *op = p;
  double result = strtod_i(p, &p);
  if (*p == '.') {
    const char *q = p + 1;
    double frac = strtod_i(q, &p);
    result += frac * ipow(10, q - p);
  }
  if (*p == 'e' || *p == 'E') {
    const char *q = p + 1;
    bool neg2 = parse_sign(&q);
    double order = strtod_i(q, &p);
    if (q == p) {
      // Error.
    } else {
      result *= ipow(10, neg2 ? -order : order);
    }
  }
  if (p == op)
    p = orig;
  if (neg)
    result = -result;

  if (pp != 0)
    *pp = (char*)p;

  return result;
}
#endif

static FILE _stdin = {.fd = STDIN_FILENO};
static FILE _stdout = {.fd = STDOUT_FILENO};
static FILE _stderr = {.fd = STDERR_FILENO};
FILE *stdin = &_stdin;
FILE *stdout = &_stdout;
FILE *stderr = &_stderr;

int fileno(FILE *fp) {
  return fp->fd;
}

size_t fwrite(const void *buffer, size_t size, size_t count, FILE *fp) {
  write(fp->fd, buffer, size * count);
}

size_t fread(void *buffer, size_t size, size_t count, FILE *fp) {
  return read(fp->fd, buffer, size * count);
}

int vfprintf(FILE *fp, const char *fmt, va_list ap) {
  // TODO: directly output to fd, not use vsnprintf.
  char buf[1024];
  int len = vsnprintf(buf, sizeof(buf), fmt, ap);
  return write(fileno(fp), buf, len);
}

int fprintf(FILE *fp, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int len = vfprintf(fp, fmt, ap);
  va_end(ap);
  return len;
}

int printf(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int len = vfprintf(stdout, fmt, ap);
  va_end(ap);
  return len;
}

int atoi(const char *s) {
  int n = 0;
  for (; '0' <= *s && *s <= '9'; ++s)
    n = n * 10 + (*s - '0');
  return n;
}

//

int isdigit(int c) {
  return '0' <= c && c <= '9';
}

int isxdigit(int c) {
  return ('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f');
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

int tolower(int c) {
  return ('A' <= c && c <= 'Z') ? c + ('a' - 'A') : c;
}

int toupper(int c) {
  return ('a' <= c && c <= 'z') ? c - ('a' - 'A') : c;
}

#if defined(__XV6)

#elif defined(__linux__)
// fcntl.h
#define	O_RDONLY        0x0000          /* open for reading only */
#define	O_WRONLY        0x0001          /* open for writing only */
#define	O_RDWR          0x0002          /* open for reading and writing */
#define	O_ACCMODE       0x0003          /* mask for above modes */

#define S_IRUSR         (0400)
#define S_IWUSR         (0200)
#define S_IXUSR         (0100)
#define S_IRGRP         (0040)
#define S_IWGRP         (0020)
#define S_IXGRP         (0010)
#define S_IROTH         (0004)
#define S_IWOTH         (0002)
#define S_IXOTH         (0001)

#define	O_APPEND        0x0400          /* set append mode */
#define	O_CREAT         0x0040          /* create if nonexistant */
#define	O_TRUNC         0x0200          /* truncate to zero length */

//extern char **environ;
//char *environ[] = {"PATH=/bin:/usr/bin", NULL};
char **environ = NULL;

int open(const char *fn, int flag, ...) {
  __asm("mov $2, %eax");  // __NR_open
  __asm("syscall");
}

int close(int fd) {
  __asm("mov $3, %eax");  // __NR_close
  __asm("syscall\n");
}

size_t read(int fd, void *buf, size_t size) {
  __asm("mov $0, %eax");  // __NR_read
  __asm("syscall");
}

static size_t _getcwd(char *buffer, size_t size) {
  __asm("mov $79, %eax");  // __NR_getcwd
  __asm("syscall");
}

pid_t fork(void) {
  __asm("mov $57, %eax");  // __NR_fork
  __asm("syscall");
}

int pipe(int *pipefd) {
  __asm("mov $22, %eax");  // __NR_pipe
  __asm("syscall");
}

int dup(int fd) {
  __asm("mov $32, %eax");  // __NR_dup
  __asm("syscall");
}

int execve(const char *path, char *const args[], char *const envp[]) {
  __asm("mov $59, %eax");  // __NR_execve
  __asm("syscall");
}

pid_t wait4(pid_t pid, int* status, int options, struct rusage *usage) {
  __asm("mov %rcx, %r10");  // 4th parameter for syscall is `%r10`. `%r10` is caller save so no need to save/restore
  __asm("mov $61, %eax");  // __NR_wait4
  __asm("syscall");
}

int chmod(const char *pathname, /*mode_t*/int mode) {
  __asm("mov $90, %eax");  // __NR_chmod
  __asm("syscall");
}

off_t lseek(int fd, off_t offset, int whence) {
  __asm("mov $8, %eax");  // __NR_lseek
  __asm("syscall\n");
}

int kill(pid_t pid, int sig) {
  __asm("mov $62, %eax");  // __NR_kill
  __asm("syscall");
}

static void *_brk(void *addr) {
  __asm("mov $12, %eax");  // __NR_brk
  __asm("syscall");
}

int unlink(const char *pathname) {
  __asm("mov $87, %eax");  // __NR_unlink
  __asm("syscall");
}

int ioctl(int fd, int request, ...) {
  __asm("mov $16, %eax");  // __NR_ioctl
  __asm("syscall");
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
  return ".";
}

char *basename(char *path) {
  char *p = strrchr(path, '/');
  if (p != NULL)
    return p + 1;
  else
    return path;
}

FILE *fopen(const char *fileName, const char *mode) {
  struct {
    const char *str;
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

  int mod = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
  int fd = open(fileName, flag, mod);
  if (fd < 0) {
    return NULL;
  }

  FILE *fp = malloc(sizeof(*fp));
  if (fp == NULL) {
    close(fd);
    return NULL;
  }

  fp->fd = fd;
  return fp;
}

int fclose(FILE *fp) {
  if (fp == NULL || fp->fd < 0)
    return EOF;
  close(fp->fd);
  fp->fd = -1;
  free(fp);
  return 0;
}

int fseek(FILE *fp, long offset, int origin) {
  return lseek(fp->fd, offset, origin);
}

long ftell(FILE *fp) {
  return fseek(fp, 0, SEEK_CUR);
}

int fgetc(FILE *fp) {
  unsigned char c;
  int len = read(fp->fd, &c, 1);
  return len == 1 ? c : EOF;
}

int fputc(int c, FILE *fp) {
  unsigned char cc = c;
  int len = write(fp->fd, &cc, 1);
  return len == 1 ? c : EOF;
}

int getchar(void) {
  return fgetc(stdin);
}

int remove(const char *fn) {
  return unlink(fn);
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

pid_t waitpid(pid_t pid, int *status, int options) {
  return wait4(pid, status, options, NULL);
}

void perror(const char *msg) {
  fprintf(stderr, "perror: %s\n", msg);
}

#if !defined(__XV6)
static char *curbrk;
int brk(void *addr) {
  void *result = _brk(addr);
  curbrk = result;
  if (result < addr)
    return EOF;
  return 0;
}

void *sbrk(intptr_t increment) {
  char *p = curbrk;
  if (p == NULL)
    p = _brk(NULL);
  char *next = p + increment;
  if (brk(next) < 0)
    return (void*)-1;
  return p;
}

#include <sys/ioctl.h>

int isatty(int fd) {
  struct termio tm;
  return ioctl(fd, TCGETA, &tm) == 0 ? 1 : 0;
}
#endif

#endif  // !USE_LIBC
