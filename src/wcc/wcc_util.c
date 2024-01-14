#include "wcc.h"

#include <assert.h>
#include <stdlib.h>  // realloc, free
#include <string.h>

#include "util.h"

void data_release(DataStorage *data) {
  if (data->chunk_stack != NULL) {
    free_vector(data->chunk_stack);
    data->chunk_stack = NULL;
  }
  if (data->buf != NULL) {
    free(data->buf);
    data_init(data);
  }
}

void data_init(DataStorage *data) {
  data->chunk_stack = NULL;
  data->buf = NULL;
  data->capacity = 0;
  data->len = 0;
}

void data_reserve(DataStorage *data, size_t capacity) {
  if (data->capacity < capacity) {
    const size_t MIN = 16;
    size_t c = data->capacity << 1;
    if (c > capacity)
      capacity = c;
    if (MIN > capacity)
      capacity = MIN;
    data->buf = realloc_or_die(data->buf, sizeof(*data->buf) * capacity);
    data->capacity = capacity;
  }
}

void data_insert(DataStorage *data, ssize_t _pos, const unsigned char *buf, size_t size) {
  size_t pos = _pos == -1 ? data->len : (size_t)_pos;
  assert(/* 0 <= pos && */ pos <= data->len);
  size_t newlen = data->len + size;
  data_reserve(data, newlen);
  if (pos < data->len)
    memmove(data->buf + pos + size, data->buf + pos, data->len - pos);
  memcpy(data->buf + pos, buf, size);
  data->len = newlen;
}

void data_append(DataStorage *data, const unsigned char *buf, size_t size) {
  data_insert(data, -1, buf, size);
}

void data_push(DataStorage *data, unsigned char c) {
  unsigned char buf[1] = {c};
  data_insert(data, -1, buf, 1);
}

void data_concat(DataStorage *dst, DataStorage *src) {
  data_insert(dst, -1, src->buf, src->len);
}

void data_leb128(DataStorage *data, ssize_t pos, int64_t val) {
  unsigned char buf[12], *p = buf;
  const int64_t MAX = 1 << 6;
  for (;;) {
    if (val < MAX && val >= -MAX) {
      *p++ = val & 0x7f;
      data_insert(data, pos, buf, p - buf);
      return;
    }
    *p++ = (val & 0x7f) | 0x80;
    val >>= 7;
  }
}

void data_uleb128(DataStorage *data, ssize_t pos, uint64_t val) {
  unsigned char buf[12], *p = buf;
  const uint64_t MAX = 1 << 7;
  for (;;) {
    if (val < MAX) {
      *p++ = val & 0x7f;
      data_insert(data, pos, buf, p - buf);
      return;
    }
    *p++ = (val & 0x7f) | 0x80;
    val >>= 7;
  }
}

void data_string(DataStorage *data, const void *str, size_t len) {
  data_uleb128(data, -1, len);
  data_append(data, (const unsigned char*)str, len);
}

void data_open_chunk(DataStorage *data) {
  Vector *stack = data->chunk_stack;
  if (stack == NULL)
    data->chunk_stack = stack = new_vector();
  vec_push(stack, INT2VOIDP(data->len));
}

void data_close_chunk(DataStorage *data, ssize_t num) {
  Vector *stack = data->chunk_stack;
  assert(stack != NULL && stack->len > 0);
  size_t pos = VOIDP2INT(vec_pop(stack));
  if (num == (ssize_t)-1)
    num = data->len - pos;
  data_uleb128(data, pos, num);
}
