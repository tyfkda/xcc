#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdio.h>  // FILE
#include <sys/types.h>  // ssize_t

#define MIN(a, b)  ((a) < (b) ? (a) : (b))
#define MAX(a, b)  ((a) > (b) ? (a) : (b))
#define ALIGN(x, align)  (((x) + (align) - 1) & -(align))  // align must be 2^n

char *strdup_(const char *str);
char *strndup_(const char *str, size_t size);
char *alloc_label(void);
char *cat_path(const char *base_dir, const char *rel_path);
ssize_t getline_(char **lineptr, size_t *n, FILE *stream, size_t start);
char *abspath(const char *root, const char *path);

void error(const char* fmt, ...) /*__attribute((noreturn))*/;

// Container

typedef struct Vector {
  void **data;
  int capacity;
  int len;
} Vector;

Vector *new_vector(void);
void vec_push(Vector *vec, const void *elem);

typedef struct Map {
  Vector *keys;
  Vector *vals;
} Map;

Map *new_map(void);
int map_count(Map *map);
void map_put(Map *map, const char *key, const void *val);
void *map_get(Map *map, const char *key);
bool map_try_get(Map *map, const char *key, void **output);
