#include "stdio.h"
#include "unistd.h"

#include "_file.h"

static FILE _stdin = {.fd = STDIN_FILENO};
static FILE _stdout = {.fd = STDOUT_FILENO};
static FILE _stderr = {.fd = STDERR_FILENO};
FILE *stdin = &_stdin;
FILE *stdout = &_stdout;
FILE *stderr = &_stderr;
