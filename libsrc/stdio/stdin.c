#include "stdio.h"
#include "unistd.h"

#include "_file.h"
#include "_fileman.h"

#define IOF  &_kFileCookieIoFunctions

typedef struct INFILE {
  FILE file;
  unsigned char rwork[256];
} INFILE;

typedef struct OUTFILE {
  FILE file;
  unsigned char wwork[128];
} OUTFILE;

static INFILE  _stdin  = {.file={.iof = IOF, .flush = _fflush, .fd = STDIN_FILENO,  .flag = FF_READ, .rbuf = _stdin.rwork, .rcapa = sizeof(_stdin.rwork)}};
static OUTFILE _stdout = {.file={.iof = IOF, .flush = _fflush, .fd = STDOUT_FILENO, .flag = FF_WRITE, .wbuf = _stdout.wwork, .wcapa = sizeof(_stdout.wwork)}};
static OUTFILE _stderr = {.file={.iof = IOF, .flush = _fflush, .fd = STDERR_FILENO, .flag = FF_WRITE, .wbuf = _stderr.wwork, .wcapa = sizeof(_stderr.wwork)}};

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
