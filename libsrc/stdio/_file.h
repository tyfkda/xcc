#pragma once

#include "stddef.h"  // size_t
#include "sys/types.h"  // ssize_t, off64_t

#define FF_BINARY   (1 << 0)
#define FF_READ     (1 << 1)
#define FF_WRITE    (1 << 2)
#define FF_EOF      (1 << 3)
#define FF_GROWMEM  (1 << 4)

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
  unsigned char *wbuf;

  int fd;
  unsigned int rp, rs;
  unsigned int wp, ws;
  unsigned int flag;

  // TODO: allocate buffers only if required.
  union {
    struct {
      unsigned char rbuf[256];
      unsigned char wwork[32];
    };  // For file.
    struct {
      char **pmem;
      size_t *psize;
    };  // For memory.
  };
};

extern ssize_t _fread(void *cookie, char *buf, size_t size);
extern ssize_t _fwrite(void *cookie, const char *buf, size_t size);
extern int _fseek(void *cookie, off_t *offset, int origin);
extern int _fclose(void *cookie);

extern void _remove_opened_file(FILE *fp);

#define FPUTC(c, fp)  ({ \
  unsigned char _buf = (c); \
  ssize_t _result = ((fp)->iof->write)((fp), (char*)&_buf, sizeof(_buf)); \
  _result == sizeof(_buf) ? (int)_buf : EOF; \
})
