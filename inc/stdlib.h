#pragma once

#include "stddef.h"  // size_t

int atoi(const char* s);
void* malloc(size_t size);
void free(void* ptr);
void* realloc(void* ptr, size_t size);
void* calloc(size_t size, size_t n);

void exit(int code);
