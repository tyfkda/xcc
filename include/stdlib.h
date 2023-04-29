#pragma once

#include <stddef.h>  // size_t

#define RAND_MAX  (0x7fffffff)

#define EXIT_SUCCESS  0
#define EXIT_FAILURE  1

int atoi(const char* s);
long atol(const char* s);
long long atoll(const char* s);
void *malloc(size_t size);
void free(void* ptr);
void *realloc(void* ptr, size_t size);
void *calloc(size_t size, size_t n);

_Noreturn void exit(int code);
int atexit(void (*func)(void));

long strtol(const char *p, char **pp, int base);
unsigned long strtoul(const char *p, char **pp, int base);
long long strtoll(const char *p, char **pp, int base);
unsigned long long strtoull(const char *p, char **pp, int base);

int abs(int x);
long labs(long x);
long long llabs(long long x);

void qsort(void *base, size_t nmemb, size_t size, int (*compare)(const void *, const void *));
void *bsearch(const void *key, const void *base, size_t nmemb, size_t size,
              int (*compare)(const void*, const void*));

int rand(void);  // [0, RAND_MAX]
void srand(unsigned int seed);

#ifndef __NO_FLONUM
double atof(const char *p);
double strtod(const char* restrict p, char ** restrict pp);
long double strtold(const char* restrict p, char ** restrict pp);
double drand48(void);                     // [0.0, 1.0)
double erand48(unsigned short xsubi[3]);  // [0.0, 1.0)
#endif
long lrand48(void);                       // [0, 1<<31)
long nrand48(unsigned short xsubi[3]);    // [0, 1<<31)
void srand48(long seedval);

char *realpath(const char *path, char *resolved_path);

int mkstemp(char *tmpl);
int mkstemps(char *tmpl, int suffixlen);

char *getenv(const char *varname);
