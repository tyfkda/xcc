#pragma once

#include <stddef.h>  // size_t

int atoi(const char* s);
long atol(const char* s);
long long atoll(const char* s);
void *malloc(size_t size);
void free(void* ptr);
void *realloc(void* ptr, size_t size);
void *calloc(size_t size, size_t n);

void exit(int code);
int atexit(void (*func)(void));

long strtol(const char *p, char **pp, int base);
unsigned long strtoul(const char *p, char **pp, int base);
long long strtoll(const char *p, char **pp, int base);
unsigned long long strtoull(const char *p, char **pp, int base);

void qsort(void *base, size_t nmemb, size_t size, int (*compare)(const void *, const void *));

#ifndef __NO_FLONUM
double atof(const char* p);
double strtod(const char* /*restrict*/ p, char ** /*restrict*/ pp);
double drand48(void);
double erand48(unsigned short xsubi[3]);
#endif

int mkstemp(char *template);
int mkstemps(char *template, int suffixlen);
