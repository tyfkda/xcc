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

int isalnum_(int c);
int isutf8first(int c);
int isutf8follow(int c);
int isoctal(int c);
int xvalue(char c);
bool starts_with(const char *str, const char *prefix);
int most_significant_bit(size_t x);
void *malloc_or_die(size_t size);
void *realloc_or_die(void *ptr, size_t size);
const Name *alloc_label(void);
ssize_t getline_chomp(char **lineptr, size_t *n, FILE *stream);
ssize_t getline_cont(char **lineptr, size_t *n, FILE *stream, int *plineno);
bool is_fullpath(const char *filename);
char *join_paths(const char *paths[]);
#define JOIN_PATHS(...)  join_paths((const char*[]){__VA_ARGS__, NULL})
char *get_ext(const char *filename);
char *change_ext(const char *path, const char *ext);
void put_padding(FILE *fp, uintptr_t start);

void show_version(const char *exe);

void error(const char *fmt, ...) /*__attribute((noreturn))*/;
void show_error_line(const char *line, const char *p, int len);

bool is_im8(intptr_t x);
bool is_im16(intptr_t x);
bool is_im32(intptr_t x);
const char *skip_whitespaces(const char *s);
int64_t wrap_value(int64_t value, int size, bool is_unsigned);

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
void free_vector(Vector *vec);
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
void sb_insert(StringBuffer *sb, int pos, const char *start, const char *end);
#define sb_append(sb, start, end)  sb_insert(sb, (sb)->elems->len, start, end)
#define sb_prepend(sb, start, end)  sb_insert(sb, 0, start, end)
#define sb_to_string(sb)  sb_join(sb, NULL)
char *sb_join(StringBuffer *sb, const char *separator);

void escape_string(const char *str, size_t size, StringBuffer *sb);

// Optparse

#define no_argument        (0)
#define required_argument  (1)

struct option {
  const char *name;
  int has_arg;
  int val;
};

extern int optind, opterr, optopt;
extern char *optarg;

int optparse(int argc, char *const argv[], const struct option *opts);
