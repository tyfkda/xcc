#include "wcc.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>

void data_release(DataStorage *data) {
  if (data->buf != NULL) {
    free(data->buf);
    data_init(data);
  }
}

void data_init(DataStorage *data) {
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
    data->buf = realloc(data->buf, sizeof(*data->buf) * capacity);
    data->capacity = capacity;
  }
}

void data_insert(DataStorage *data, size_t pos, const unsigned char *buf, size_t size) {
  assert(/* 0 <= pos && */ pos <= data->len);
  size_t newlen = data->len + size;
  data_reserve(data, newlen);
  if (pos < data->len)
    memmove(data->buf + pos + size, data->buf + pos, data->len - pos);
  memcpy(data->buf + pos, buf, size);
  data->len = newlen;
}

void data_append(DataStorage *data, const unsigned char *buf, size_t size) {
  data_insert(data, data->len, buf, size);
}

void data_push(DataStorage *data, unsigned char c) {
  unsigned char buf[1] = {c};
  data_insert(data, data->len, buf, 1);
}

void data_concat(DataStorage *data, DataStorage *data2) {
  data_insert(data, data->len, data2->buf, data2->len);
}
