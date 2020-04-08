#pragma once

#include <stddef.h>  // size_t

size_t strlen(const char *s);
char* strchr(const char *s, int c);
char* strrchr(const char *s, int c);
int strcmp(const char *p, const char *q);
int strncmp(const char *p, const char *q, size_t n);
char* strcpy(char *s, const char *t);
char* strncpy(char *s, const char *t, size_t n);
long strtol(const char *p, char **pp, int base);

void* memcpy(void *dst, const void *src, size_t n);
void* memmove(void*, const void*, size_t);
void* memset(void* buf, int val, size_t size);
int memcmp(const void *buf1, const void *buf2, size_t n);
