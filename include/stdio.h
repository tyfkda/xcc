#pragma once

#include <stdarg.h>
#include <stddef.h>
#include <sys/types.h>  // ssize_t

#define EOF  (-1)

#define BUFSIZ  1024
#define	L_tmpnam  1024

enum {
  SEEK_SET,  // 0
  SEEK_CUR,  // 1
  SEEK_END,  // 2
};

typedef struct FILE FILE;

#ifdef __APPLE__
extern FILE *__stdinp;
extern FILE *__stdoutp;
extern FILE *__stderrp;
#define stdin   __stdinp
#define stdout  __stdoutp
#define stderr  __stderrp

#elif defined(__riscv)

// Must match with newlib
struct _reent
{
  int _errno;

  struct FILE *_stdin, *_stdout, *_stderr;
};

extern struct _reent *_impure_ptr;

#define stdin   (_impure_ptr->_stdin)
#define stdout  (_impure_ptr->_stdout)
#define stderr  (_impure_ptr->_stderr)

#else
extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;
#endif

FILE *fopen(const char *fileName, const char *mode);
FILE *fdopen(int fd, const char *mode);
int fclose(FILE *fp);
size_t fwrite(const void *buffer, size_t size, size_t count, FILE *fp);
size_t fread(void *buffer, size_t size, size_t count, FILE *fp);
int fflush(FILE *fp);
int fseek(FILE *fp, long offset, int origin);
long ftell(FILE *fp);
int feof(FILE *fp);
int ferror(FILE *fp);
void clearerr(FILE *fp);
FILE *freopen(const char *path, const char *mode, FILE *fp);
int remove(const char *fn);
int rename(const char *oldname, const char *newname);
char *tmpnam(char *);

int fgetc(FILE *fp);
int fputc(int c, FILE *fp);
char *fgets(char *s, int n, FILE *fp);
int fputs(const char *s, FILE *fp);
int puts(const char *s);
int ungetc(int c, FILE *fp);

#define getc(fp)     fgetc(fp)
#define getchar()    fgetc(stdin)
#define putc(c, fp)  fputc(c, fp)
#define putchar(c)   fputc(c, stdout)

int fprintf(FILE *fp, const char *fmt, ...);
int printf(const char *fmt, ...);
int sprintf(char *out, const char *fmt, ...);
int snprintf(char *, size_t n, const char *, ...);
int vprintf(const char *fmt, va_list ap);
int vsprintf(char *buf, const char *fmt, va_list ap);
int vfprintf(FILE *fp, const char *fmt, va_list ap);
int vsnprintf(char *out, size_t n, const char *fmt_, va_list ap);

void perror(const char *);

int fileno(FILE *fp);
FILE *tmpfile(void);

ssize_t getline(char **lineptr, size_t *n, FILE *stream);

FILE *fmemopen(void *buf, size_t size, const char *mode);
FILE *open_memstream(char **ptr, size_t *sizeloc);

#define	_IOFBF	0		/* setvbuf should set fully buffered */
#define	_IOLBF	1		/* setvbuf should set line buffered */
#define	_IONBF	2		/* setvbuf should set unbuffered */

int	setvbuf(FILE *fp, char *, int, size_t);
