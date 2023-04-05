#include "stdio.h"
#include "unistd.h"

#include "_file.h"
#include "_fileman.h"

static FILE _stdin = {.fd = STDIN_FILENO};
static FILE _stdout = {.fputc = _fputc, .fd = STDOUT_FILENO, .wbuf = _stdout.wwork, .ws = sizeof(_stdout.wwork)};
static FILE _stderr = {.fputc = _fputc, .fd = STDERR_FILENO, .wbuf = _stderr.wwork, .ws = sizeof(_stderr.wwork)};
FILE *stdin = &_stdin;
FILE *stdout = &_stdout;
FILE *stderr = &_stderr;

FILEMAN __fileman;
