// Utility

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdio.h>  // FILE
#include <sys/types.h>  // ssize_t
#include <stdint.h>

#include "../config.h"

#define MIN(a, b)  ((a) < (b) ? (a) : (b))
#define MAX(a, b)  ((a) > (b) ? (a) : (b))
#define ALIGN(x, align)  (((x) + (align) - 1) & -(align))  // align must be 2^n
#define UNUSED(x)  ((void)(x))
#define IS_POWER_OF_2(x)  ((x) > 0 && ((x) & ((x) - 1)) == 0)
#define INT2VOIDP(i)   ((void*)(intptr_t)(i))
#define UINT2VOIDP(i)  ((void*)(uintptr_t)(i))
#define VOIDP2INT(p)   ((intptr_t)(p))
#define VOIDP2UINT(p)  ((uintptr_t)(p))
#define ARRAY_SIZE(array)  (sizeof(array) / sizeof(*(array)))

typedef struct Name Name;

int isalnum_(int c);
int isutf8first(int c);
int isutf8follow(int c);
int isoctal(int c);
int xvalue(char c);
bool starts_with(const char *str, const char *prefix);
int most_significant_bit(size_t x);
void read_or_die(FILE *fp, void *buf, size_t size, const char *msg);
void *malloc_or_die(size_t size);
void *calloc_or_die(size_t size);  // No `count` argument.
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
bool is_file(const char *path);

void show_version(const char *exe);

void error(const char *fmt, ...) __attribute__((noreturn));

void show_error_line(const char *line, const char *p, int len);

bool is_im8(intptr_t x);
bool is_im16(intptr_t x);
bool is_im32(intptr_t x);
const char *skip_whitespaces(const char *s);
const char *block_comment_start(const char *p);
const char *block_comment_end(const char *p);
int64_t wrap_value(int64_t value, int size, bool is_unsigned);

// Container

typedef struct Vector {
  void **data;
  int capacity;
  int len;
} Vector;

Vector *new_vector(void);
void free_vector(Vector *vec);
void vec_init(Vector *vec);
void vec_clear(Vector *vec);
void vec_push(Vector *vec, const void *elem);
void *vec_pop(Vector *vec);
void vec_insert(Vector *vec, int pos, const void *elem);
void vec_remove_at(Vector *vec, int index);
bool vec_contains(Vector *vec, void *elem);

// DataStorage

typedef struct DataStorage {
  Vector *chunk_stack;
  unsigned char *buf;
  size_t capacity;
  size_t len;
} DataStorage;

void data_release(DataStorage *data);
void data_init(DataStorage *data);
void data_reserve(DataStorage *data, size_t capacity);
void data_insert(DataStorage *data, ssize_t pos, const unsigned char *buf, size_t size);
void data_append(DataStorage *data, const unsigned char *buf, size_t size);
void data_push(DataStorage *data, unsigned char c);
void data_align(DataStorage *data, int align);
void data_concat(DataStorage *dst, DataStorage *src);
void data_leb128(DataStorage *data, ssize_t pos, int64_t val);
void data_uleb128(DataStorage *data, ssize_t pos, uint64_t val);
void data_string(DataStorage *data, const void *str, size_t len);
void data_open_chunk(DataStorage *data);
void data_close_chunk(DataStorage *data, ssize_t num);
void data_varuint32(DataStorage *data, ssize_t pos, uint64_t val);

// StringBuffer

typedef struct StringBuffer {
  Vector *elems;
} StringBuffer;

void sb_init(StringBuffer *sb);
void sb_clear(StringBuffer *sb);
bool sb_empty(StringBuffer *sb);
void sb_insert(StringBuffer *sb, int pos, const char *start, const char *end);
inline void sb_append(StringBuffer *sb, const char *start, const char *end)  { sb_insert(sb, sb->elems->len, start, end); }
inline void sb_prepend(StringBuffer *sb, const char *start, const char *end)  { sb_insert(sb, 0, start, end); }
char *sb_join(StringBuffer *sb, const char *separator);
inline char *sb_to_string(StringBuffer *sb)  { return sb_join(sb, NULL); }

void escape_string(const char *str, size_t size, StringBuffer *sb);

// Optparse

#define no_argument        (0)
#define required_argument  (1)
#define optional_argument  (2)

struct option {
  const char *name;
  int has_arg;
  int val;
};

extern int optind, opterr, optopt;
extern char *optarg;

int optparse(int argc, char *const argv[], const struct option *opts);
