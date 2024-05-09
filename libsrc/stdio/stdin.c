#include "stdio.h"
#include "unistd.h"

#include "_file.h"
#include "_fileman.h"

#define IOF  &_kFileCookieIoFunctions

static FILE _stdin = {.iof = IOF, .flush = _fflush, .fd = STDIN_FILENO, .flag = FF_READ};
static FILE _stdout = {.iof = IOF, .flush = _fflush, .fd = STDOUT_FILENO, .flag = FF_WRITE, .wbuf = _stdout.wwork, .ws = sizeof(_stdout.wwork)};
static FILE _stderr = {.iof = IOF, .flush = _fflush, .fd = STDERR_FILENO, .flag = FF_WRITE, .wbuf = _stderr.wwork, .ws = sizeof(_stderr.wwork)};

#if defined(__riscv)
static struct _reent _impure_entity = {
  ._errno = 0,
  ._stdin = &_stdin,
  ._stdout = &_stdout,
  ._stderr = &_stderr,
};
struct _reent *_impure_ptr = &_impure_entity;

#else

FILE *stdin = &_stdin;
FILE *stdout = &_stdout;
FILE *stderr = &_stderr;
#endif

FILEMAN __fileman;
