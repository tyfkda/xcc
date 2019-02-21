#pragma once

#include <stddef.h>  // size_t
#include <stdio.h>  // FILE
#include <sys/types.h>  // ssize_t

char *strdup_(const char *str);
char *strndup_(const char *str, size_t size);
ssize_t getline_(char **lineptr, size_t *n, FILE *stream);

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
