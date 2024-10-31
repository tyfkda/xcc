#include "stdio.h"

#include "fcntl.h"  // O_ACCMODE, etc.
#include "stdlib.h"  // calloc
#include "string.h"  // strchr
#include "unistd.h"  // close

#include "_file.h"

#define RCAPA  256
#define WCAPA  128

static int _fclose(void *cookie) {
  FILE *fp = cookie;
  _remove_opened_file(fp);
  (*fp->flush)(fp);
  close(fp->fd);
  free(fp);
  return 0;
}

FILE *fdopen(int fd, const char *mode) {
  // TODO: Validate fd.

  int flag = 0;
  int oflag = _detect_open_flag(mode);
  size_t size = sizeof(FILE);
  switch (oflag & O_ACCMODE) {
  case O_RDONLY:  flag |= FF_READ; size += RCAPA; break;
  case O_WRONLY:  flag |= FF_WRITE; size += WCAPA; break;
  case O_RDWR:    flag |= FF_READ | FF_WRITE; size += RCAPA + WCAPA; break;
  default: break;
  }
  if (strchr(mode, 'b') != NULL)
    flag |= FF_BINARY;

  FILE *fp = calloc(1, size);
  if (fp != NULL) {
    static const cookie_io_functions_t kIof = {
      .read = _fread,
      .write = _fwrite,
      .seek = _fseek,
      .close = _fclose,
    };

    fp->iof = &kIof;
    fp->flush = _fflush;
    fp->fd = fd;
    fp->rp = fp->rs = fp->rcapa = 0;
    fp->wp = fp->wcapa = 0;
    fp->rbuf = NULL;
    fp->wbuf = NULL;
    fp->flag = flag;
    fp->unget_char = EOF;

    unsigned char *p = (unsigned char*)(fp + 1);
    if (flag & FF_READ) {
      fp->rbuf = p;
      p += (fp->rcapa = RCAPA);
    }
    if (flag & FF_WRITE) {
      fp->wbuf = p;
      p += (fp->wcapa = WCAPA);
    }

    _add_opened_file(fp);
  }
  return fp;
}
