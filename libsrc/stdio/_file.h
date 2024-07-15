#pragma once

#include "stddef.h"  // size_t
#include "sys/types.h"  // ssize_t, off64_t

#define FF_BINARY       (1 << 0)
#define FF_READ         (1 << 1)
#define FF_WRITE        (1 << 2)
#define FF_EOF          (1 << 3)
#define FF_GROWMEM      (1 << 4)
#define FF_FIFO         (1 << 5)
#define FF_INITIALIZED  (1 << 15)

typedef ssize_t (*cookie_read_function_t)(void *cookie, char *buf, size_t size);
typedef ssize_t (*cookie_write_function_t)(void *cookie, const char *buf, size_t size);
typedef int (*cookie_seek_function_t)(void *cookie, off_t *offset, int whence);
typedef int (*cookie_close_function_t)(void *cookie);

typedef struct {
  cookie_read_function_t read;
  cookie_write_function_t write;
  cookie_seek_function_t seek;
  cookie_close_function_t close;
} cookie_io_functions_t;

extern const cookie_io_functions_t _kFileCookieIoFunctions;

struct FILE {
  const cookie_io_functions_t *iof;  // not struct but const pointer.
  int (*flush)(struct FILE *fp);
  unsigned char *wbuf;
  unsigned char *rbuf;

  int fd;
  unsigned int rp, rs, rcapa;
  unsigned int wp, wcapa;
  unsigned int flag;
};

extern int _fflush(FILE *fp);

extern void _add_opened_file(FILE *fp);
extern void _remove_opened_file(FILE *fp);
extern int _detect_open_flag(const char *mode);

inline int FPUTC(int c, FILE *fp)  {
  unsigned char _buf = c;
  ssize_t _result = (fp->iof->write)(fp, (char*)&_buf, sizeof(_buf));
  return _result == sizeof(_buf) ? (int)_buf : EOF;
}
