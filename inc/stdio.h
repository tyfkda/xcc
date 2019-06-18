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

int fgetc(FILE* fp);
int fputc(int c, FILE* fp);

size_t fprintf(FILE *fp, const char *fmt, ...);
size_t printf(const char *fmt, ...);
size_t sprintf(char *out, const char *fmt, ...);
size_t snprintf(char*, size_t n, const char*, ...);
size_t vfprintf(FILE *fp, const char *fmt, va_list ap);

void perror(const char*);
