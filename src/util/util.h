// Utility

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>  // intptr_t
#include <stdio.h>  // FILE
#include <sys/types.h>  // ssize_t

#define MIN(a, b)  ((a) < (b) ? (a) : (b))
#define MAX(a, b)  ((a) > (b) ? (a) : (b))
#define ALIGN(x, align)  (((x) + (align) - 1) & -(align))  // align must be 2^n
#define UNUSED(x)  ((void)(x))
#define IS_POWER_OF_2(x)  (x > 0 && (x & (x - 1)) == 0)

typedef struct Name Name;

char *strdup_(const char *str);
char *strndup_(const char *str, size_t size);
bool starts_with(const char *str, const char *prefix);
void set_local_label_prefix(const char *prefix);
ssize_t getline_(char **lineptr, size_t *n, FILE *stream, size_t start);
char *cat_path(const char *root, const char *path);
char *change_ext(const char *path, const char *ext);

void myqsort(void *base, size_t nmemb, size_t size, int (*compare)(const void *, const void *));

void show_version(const char *exe);

void error(const char *fmt, ...) /*__attribute((noreturn))*/;

bool is_im8(intptr_t x);
bool is_im32(intptr_t x);

// Container

typedef struct Buffer {
  unsigned char *data;
  size_t capa;
  size_t size;
} Buffer;

void buf_put(Buffer *buf, const void *data, size_t bytes);
void buf_align(Buffer *buf, int align);

typedef struct Vector {
  void **data;
  int capacity;
  int len;
} Vector;

Vector *new_vector(void);
void vec_clear(Vector *vec);
void vec_push(Vector *vec, const void *elem);
void *vec_pop(Vector *vec);
void vec_insert(Vector *vec, int pos, const void *elem);
void vec_remove_at(Vector *vec, int index);
bool vec_contains(Vector *vec, void *elem);

// StringBuffer

typedef struct StringBuffer {
  Vector *elems;
} StringBuffer;

void sb_init(StringBuffer *sb);
void sb_clear(StringBuffer *sb);
bool sb_empty(StringBuffer *sb);
void sb_append(StringBuffer *sb, const char *start, const char *end);
char *sb_to_string(StringBuffer *sb);
