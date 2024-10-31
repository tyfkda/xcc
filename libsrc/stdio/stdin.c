#include "stdio.h"
#include "unistd.h"

#include "_file.h"
#include "_fileman.h"

#define FFLUSH  _fflush

typedef struct INFILE {
  FILE file;
  unsigned char rwork[256];
} INFILE;

typedef struct OUTFILE {
  FILE file;
  unsigned char wwork[128];
} OUTFILE;

// Same as _fclose, except calling _remove_opend_file and free.
static int _fclose_std(void *cookie) {
  FILE *fp = cookie;
  // _remove_opened_file(fp);
  (*fp->flush)(fp);
  close(fp->fd);
  // free(fp);
  return 0;
}

static const cookie_io_functions_t kIof = {
  .read = _fread,
  .write = _fwrite,
  .seek = _fseek,
  .close = _fclose_std,
};
#define IOF  &kIof

static INFILE  _stdin  = {.file={.iof = IOF, .flush = FFLUSH, .fd = STDIN_FILENO,  .flag = FF_READ, .rbuf = _stdin.rwork, .rcapa = sizeof(_stdin.rwork), .unget_char=EOF}};
static OUTFILE _stdout = {.file={.iof = IOF, .flush = FFLUSH, .fd = STDOUT_FILENO, .flag = FF_WRITE, .wbuf = _stdout.wwork, .wcapa = sizeof(_stdout.wwork), .unget_char=EOF}};
static OUTFILE _stderr = {.file={.iof = IOF, .flush = FFLUSH, .fd = STDERR_FILENO, .flag = FF_WRITE, .wbuf = _stderr.wwork, .wcapa = sizeof(_stderr.wwork), .unget_char=EOF}};

#if defined(__riscv)
static struct _reent _impure_entity = {
  ._errno = 0,
  ._stdin = &_stdin.file,
  ._stdout = &_stdout.file,
  ._stderr = &_stderr.file,
};
struct _reent *_impure_ptr = &_impure_entity;

#else

FILE *stdin = &_stdin.file;
FILE *stdout = &_stdout.file;
FILE *stderr = &_stderr.file;
#endif

__attribute__((destructor))
static void flush_std_files(void) {
  FFLUSH(stdout);
  FFLUSH(stderr);
}
