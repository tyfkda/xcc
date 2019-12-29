#pragma once

#include "stdarg.h"
#include "stddef.h"

#define EOF  (-1)

typedef struct FILE FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

FILE* fopen(const char* fileName, const char* mode);
int fclose(FILE* fp);
size_t fwrite(const void* buffer, size_t size, size_t count, FILE* fp);
long ftell(FILE *fp);
int remove(const char *fn);

int fgetc(FILE* fp);
int fputc(int c, FILE* fp);

int fprintf(FILE *fp, const char *fmt, ...);
int printf(const char *fmt, ...);
int sprintf(char *out, const char *fmt, ...);
int snprintf(char*, size_t n, const char*, ...);
int vfprintf(FILE *fp, const char *fmt, va_list ap);
int vsnprintf(char *out, size_t n, const char *fmt_, va_list ap);

void perror(const char*);
