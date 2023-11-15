#include "stdio.h"

#include "fcntl.h"  // open
#include "stdlib.h"  // malloc, realloc, exit
#include "string.h"  // strcmp

#include "_file.h"
#include "_fileman.h"

#define INITIAL_CAPACITY  (4)

extern FILEMAN __fileman;

// Make sure inline function is out.
extern inline int FPUTC(int c, FILE *fp);

static void add_opened(FILE *fp) {
  if (__fileman.length >= __fileman.capacity) {
    int ncapa = __fileman.capacity > 0 ? __fileman.capacity << 1 : INITIAL_CAPACITY;
    FILE **buf = realloc(__fileman.opened, ncapa * sizeof(*__fileman.opened));
    if (buf == NULL) {
      // TODO:
      fputs("Out of memory\n", stderr);
      exit(1);
    }
    __fileman.opened = buf;
    __fileman.capacity = ncapa;
  }
  __fileman.opened[__fileman.length++] = fp;
}

int _detect_open_flag(const char *mode) {
  enum Chr {
    RWMASK = 3,
    R = 1 | (1 << 5),
    W = 2 | (1 << 5),
    P = 1 << 2,
    MASK = 7,
    A = 1 << 3,
    B = 1 << 4,
  };
  enum Err {
    OK,
    CONFLICT,
    ILLEGAL_CHR,
  };

  int chr = 0, c = 0;
  enum Err err = OK;
  for (const char *p = mode; (c = *p) != '\0'; ++p) {
    switch (c) {
    case 'r':  c = R; break;
    case 'w':  c = W; break;
    case 'a':  c = W | A; break;
    case '+':  c = P; break;
    case 'b':  c = B; break;
    default:  err = ILLEGAL_CHR; break;
    }
    if (err != OK || ((chr & c) != 0 && (err = CONFLICT, 1)))
      break;
    chr |= c;
  }
  switch (err) {
  case OK: default:
    break;
  case CONFLICT:
    fprintf(stderr, "conflict: %c\n", c);
    return -1;
  case ILLEGAL_CHR:
    fprintf(stderr, "illegal character: %c\n", c);
    return -1;
  }
  if ((chr & RWMASK) == 0) {
    fprintf(stderr, "r|w|a required\n");
    return -1;
  }

  static const int kTable[] = {
    [R & MASK] = O_RDONLY,
    [W & MASK] = O_WRONLY | O_CREAT,
    [(R | P) & MASK] = O_RDWR,
    [(W | P) & MASK] = O_RDWR | O_CREAT,
  };
  int flag = kTable[chr & MASK];
  if ((chr & RWMASK) == (W & RWMASK))
    flag |= (chr & A) ? O_APPEND : O_TRUNC;
  return flag;
}

const cookie_io_functions_t _kFileCookieIoFunctions = {
  .read = _fread,
  .write = _fwrite,
  .seek = _fseek,
  .close = _fclose,
};

FILE *fdopen(int fd, const char *mode) {
  // TODO: Validate fd.

  FILE *fp = calloc(1, sizeof(*fp));
  if (fp != NULL) {
    fp->iof = &_kFileCookieIoFunctions;
    fp->flush = _fflush;
    fp->fd = fd;
    fp->rp = fp->rs = 0;
    fp->wp = 0;
    fp->wbuf = fp->wwork;
    fp->ws = sizeof(fp->wwork);

    int flag = 0;
    int oflag = _detect_open_flag(mode);
    switch (oflag & O_ACCMODE) {
    case O_RDONLY:  flag |= FF_READ; break;
    case O_WRONLY:  flag |= FF_WRITE; break;
    case O_RDWR:    flag |= FF_READ | FF_WRITE; break;
    default: break;
    }
    if (strchr(mode, 'b') != NULL)
      flag |= FF_BINARY;
    fp->flag = flag;

    add_opened(fp);
  }
  return fp;
}
