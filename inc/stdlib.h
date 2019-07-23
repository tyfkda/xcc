#pragma once

#include "stddef.h"  // size_t

int atoi(const char* s);
void *malloc(size_t size);
void free(void* ptr);
void *realloc(void* ptr, size_t size);
void *calloc(size_t size, size_t n);
void qsort(void *base, size_t nmemb, size_t size, int (*compare)(const void *, const void *));

void exit(int code);
