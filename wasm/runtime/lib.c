#include "ctype.h"
#include "fcntl.h"
#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"

#include "_file.h"

extern void *_brk(void *);
extern int _tmpfile(void);
extern int _getcwd(char *, size_t);

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
      return (char*)s1;
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
  return n == 0 ? 0 : (int)*(unsigned char*)p - (int)*(unsigned char*)q;
}

int strcasecmp(const char *p, const char *q) {
  for (;; ++p, ++q) {
    unsigned char c1 = *(unsigned char*)p;
    unsigned char c2 = *(unsigned char*)q;
    int d = (int)c1 - (int)c2;
    if (d != 0)
      return d;
    if (c1 == 0)
      break;
  }
  return 0;
}

int strncasecmp(const char *p, const char *q, size_t n) {
  for (; n > 0; --n, ++p, ++q) {
    int c1 = tolower(*(unsigned char*)p);
    int c2 = tolower(*(unsigned char*)q);
    int d = c1 - c2;
    if (d != 0)
      return d;
    if (c1 == 0)
      break;
  }
  return 0;
}

char *strcpy(char *dst, const char *src) {
  char *os = dst;
  while ((*dst++ = *src++) != '\0')
    ;
  return os;
}

char *strncpy(char *dst, const char *src, size_t n) {
  char *os = dst;
  for (; n > 0 && (*dst++ = *src++) != '\0'; --n)
    ;
  return os;
}

char *strcat(char *dst, const char *src) {
  strcpy(dst + strlen(dst), src);
  return dst;
}

char *strncat(char *dst, const char *src, size_t n) {
  char *os = dst;
  dst += strlen(dst);
  for (; n > 0 && *src != '\0'; --n)
    *dst++ = *src++;
  *dst = '\0';
  return os;
}

char *strdup(const char *str) {
  return strndup(str, strlen(str));
}

char *strndup(const char *str, size_t size) {
  char *dup = malloc(size + 1);
  memcpy(dup, str, size);
  dup[size] = '\0';
  return dup;
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

static unsigned long strtoul_sub(const char *p, char **pp, int base) {
  char digimax = '0' + (base <= 10 ? base : 10);
  char hexmax = 'a' - 10 + base;
  unsigned long result = 0;
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

  if (pp != 0)
    *pp = (char*)p;

  return result;
}

long strtol(const char *p, char **pp, int base) {
  const char *orig = p;
  bool neg = parse_sign(&p);
  char *q;
  long result = strtoul_sub(p, &q, base);
  if (q == p)
    q = (char*)orig;
  if (neg)
    result = -result;

  if (pp != 0)
    *pp = q;

  return result;
}

unsigned long strtoul(const char *p, char **pp, int base) {
  const char *orig = p;
  if (*p == '+')
    ++p;
  char *q;
  unsigned long result = strtoul_sub(p, &q, base);
  if (q == p)
    q = (char*)orig;

  if (pp != 0)
    *pp = q;

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

  static const struct {
    const char *str;
    double pos, neg;
  } CONST[] = {
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
  return write(fp->fd, buffer, size * count);
}

size_t fread(void *buffer, size_t size, size_t count, FILE *fp) {
  return read(fp->fd, buffer, size * count);
}

int vsprintf(char *buf, const char *fmt, va_list ap) {
  return vsnprintf(buf, -1UL, fmt, ap);
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

void qsort(void *base, size_t nmemb, size_t size, int (*compare)(const void *, const void *)) {
  if (nmemb <= 1)
    return;

  char *a = base;
  const char *px;

  px = &a[(nmemb >> 1) * size];
  int i = 0;
  int j = nmemb - 1;
  for (;;) {
    while (compare(&a[i * size], px) < 0)
      ++i;
    while (compare(px, &a[j * size]) < 0)
      --j;
    if (i >= j)
      break;

    char *pi = &a[i * size];
    char *pj = &a[j * size];
    for (size_t k = 0; k < size; ++k) {
      char t = pi[k];
      pi[k] = pj[k];
      pj[k] = t;
    }
    if (px == pi)
      px = pj;
    else if (px == pj)
      px = pi;
    ++i;
    --j;
  }
  if (i > 1)
    qsort(a, i, size, compare);
  if ((size_t)(j + 2) < nmemb)
    qsort(&a[(j + 1) * size], nmemb - j - 1, size, compare);
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
  static const struct {
    const char *str;
    int flag;
  } kTable[] = {
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

char *fgets(char *s, int n, FILE *fp) {
  --n;
  char *p = s;
  for (int i = 0; i < n; ++i) {
    int c = fgetc(fp);
    if (c == EOF)
      break;
    *p++ = c;
    if (c == '\n')
      break;
  }
  if (p == s)
    return NULL;
  *p = '\0';
  return s;
}

int fputs(const char *s, FILE *fp) {
  return fwrite(s, strlen(s), 1, fp) == 1 ? 1 : EOF;
}

int getc(FILE *fp) {
  return fgetc(fp);
}

int getchar(void) {
  return fgetc(stdin);
}

ssize_t getline(char **lineptr, size_t *pcapa, FILE *stream) {
  const int MIN_CAPA = 16;
  ssize_t capa = *pcapa;
  ssize_t size = 0;
  char *top = *lineptr;
  if (top == NULL || capa <= 0) {
    top = NULL;
    capa = 0;
  }
  for (;;) {
    int c = fgetc(stream);
    if (c == EOF) {
      if (size == 0)
        return -1;
      break;
    }

    if (size + 1 >= capa) {
      ssize_t newcapa = capa * 2;
      if (newcapa < MIN_CAPA)
        newcapa = MIN_CAPA;
      char *reallocated = realloc(top, newcapa);
      if (reallocated == NULL) {
        *lineptr = top;
        *pcapa = capa;
        return -1;
      }
      top = reallocated;
      capa = newcapa;
    }

    //assert(size < capa);
    top[size++] = c;

    if (c == '\n')
      break;
  }

  //assert(size < capa);
  top[size] = '\0';
  *lineptr = top;
  *pcapa = capa;
  return size;
}

char *getcwd(char *buffer, size_t size) {
  void *allocated = NULL;
  if (buffer == NULL) {
    if (size == 0) {
      size = 512;  // PATH_MAX
    }
    buffer = allocated = malloc(size + 1);
    if (buffer == NULL)
      return NULL;
  }
  int result = _getcwd(buffer, size);
  if (result < 0) {
    // errno = -result;
    free(allocated);
    return NULL;
  }
  return buffer;
}

FILE *tmpfile(void) {
  int fd = _tmpfile();
  if (fd < 0)
    return NULL;

  FILE *fp = malloc(sizeof(*fp));
  if (fp == NULL) {
    close(fd);
    return NULL;
  }

  fp->fd = fd;
  return fp;
}

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
