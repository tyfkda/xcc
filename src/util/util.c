#include "util.h"

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>  // strcmp
#include <sys/stat.h>

#include "../version.h"
#include "table.h"

int isalnum_(int c) {
  return isalnum(c) || c == '_';
}

int isutf8first(int c_) {
  unsigned int c = c_;
  if (c < 0xc0)
    return 0;  // FALSE
  if (c >= 0xfc)
    return 6;
  if (c >= 0xf8)
    return 5;
  if (c >= 0xf0)
    return 4;
  if (c >= 0xe0)
    return 3;
  return 2;
}

int isutf8follow(int c) {
  return c >= 0x80 && c < 0xc0;
}

int isoctal(int c) {
  return '0' <= c && c <= '7';
}

int xvalue(char c) {
  return ('0' <= c && c <= '9')   ? c - '0'
         : ('A' <= c && c <= 'F') ? c - ('A' - 10)
         : ('a' <= c && c <= 'f') ? c - ('a' - 10)
                                  : -1;
}

bool starts_with(const char *str, const char *prefix) {
  size_t len = strlen(prefix);
  return strncmp(str, prefix, len) == 0;
}

int most_significant_bit(size_t x) {
  int bit;
  for (bit = 0;; ++bit) {
    x >>= 1;
    if (x <= 0)
      return bit;
  }
}

void read_or_die(FILE *fp, void *buf, size_t size, const char *msg) {
  size_t count = fread(buf, size, 1, fp);
  if (count != 1)
    error(msg);
}

void *malloc_or_die(size_t size) {
  void *p = malloc(size);
  if (p == NULL) {
    fprintf(stderr, "memory overflow\n");
    exit(1);
  }
  return p;
}

void *calloc_or_die(size_t size) {
  void *p = calloc(1, size);
  if (p == NULL) {
    fprintf(stderr, "memory overflow\n");
    exit(1);
  }
  return p;
}

void *realloc_or_die(void *ptr, size_t size) {
  void *p = realloc(ptr, size);
  if (p == NULL) {
    fprintf(stderr, "memory overflow\n");
    exit(1);
  }
  return p;
}

const Name *alloc_label(void) {
  static int label_no;
  ++label_no;
  char buf[2 + sizeof(int) * 3 + 1];
  snprintf(buf, sizeof(buf), ".L%04d", label_no);
  return alloc_name(buf, NULL, true);
}

ssize_t getline_chomp(char **lineptr, size_t *n, FILE *stream) {
  ssize_t len = getline(lineptr, n, stream);
  if (len > 0) {
    char *line = *lineptr;
    // Chomp CR(\r), LF(\n), CR+LF
    if (line[len - 1] == '\n')
      line[--len] = '\0';
    if (line[len - 1] == '\r')
      line[--len] = '\0';
  }
  return len;
}

static ssize_t getline_cat(char **lineptr, size_t *n, FILE *stream, size_t curlen) {
  char *nextline = NULL;
  size_t capa = 0;
  ssize_t len = getline_chomp(&nextline, &capa, stream);
  if (len == -1)
    return -1;
  if (len > 0) {
    char *oldline = *lineptr;
    char *reallocated = realloc(oldline, curlen + len + 1);
    if (reallocated == NULL)
      return -1;

    memcpy(reallocated + curlen, nextline, len + 1);
    *lineptr = reallocated;
    *n = curlen + len;  // '\0' is not included.
    free(nextline);
  }
  return curlen + len;
}

ssize_t getline_cont(char **lineptr, size_t *capa, FILE *stream, int *plineno) {
  int lineno = *plineno;
  ssize_t len = getline_chomp(lineptr, capa, stream);
  if (len != -1) {
    // Continue line.
    while (++lineno, len > 0 && (*lineptr)[len - 1] == '\\') {
      (*lineptr)[--len] = '\0';
      ssize_t nextlen = getline_cat(lineptr, capa, stream, len);
      if (nextlen == -1)
        break;
      len = nextlen;
    }
  }
  *plineno = lineno;
  return len;
}

bool is_fullpath(const char *filename) {
  if (*filename != '/')
    return false;
  for (const char *p = filename;;) {
    p = strstr(p, "/..");
    if (p == NULL)
      return true;
    if (p[3] == '/' || p[3] == '\0')
      return false;
    p += 3;
  }
}

char *join_paths(const char *paths[]) {
  StringBuffer sb;
  sb_init(&sb);
  int parent_count = 0;
  enum Top {
    OTHER,
    ROOTDIR,  // /
    CURDIR,   // .
  };
  enum Top top = OTHER;

  const char *p;
  for (const char **pp = paths; (p = *pp++) != NULL; ) {
    if (*p == '/') {  // Root.
      sb_init(&sb);
      parent_count = 0;
      top = ROOTDIR;
    }

    for (bool end = false; !end;) {
      while (*p == '/')
        ++p;
      if (*p == '\0')
        break;

      const char *q = strchr(p, '/');
      if (q == NULL) {  // Last.
        q = p + strlen(p);
        end = true;
      }
      ptrdiff_t len = q - p;
      if (len == 1 && *p == '.') {
        if (sb.elems->len == 0 && top == OTHER)
          top = CURDIR;
      } else if (len == 2 && strncmp(p, "..", 2) == 0) {
        if (sb.elems->len > 0) {
          void *elem = vec_pop(sb.elems);
          free(elem);
        } else {
          if (top == ROOTDIR) {
            // Illegal.
            return NULL;
          }
          ++parent_count;
          top = OTHER;
        }
      } else {
        sb_append(&sb, p, q);
      }
      p = q;
    }
  }

  for (; parent_count > 0; --parent_count)
    sb_prepend(&sb, "..", NULL);
  switch (top) {
  case CURDIR:   sb_prepend(&sb, ".", NULL); break;
  case ROOTDIR:  sb_prepend(&sb, sb.elems->len > 0 ? "" : "/", NULL); break;
  case OTHER: break;
  }
  return sb_join(&sb, "/");
}

char *get_ext(const char *filename) {
  const char *last_slash = strrchr(filename, '/');
  if (last_slash == NULL)
    last_slash = filename;
  char *dot = strrchr(last_slash, '.');
  return dot != NULL ? (char*)&dot[1]: (char*)&last_slash[strlen(last_slash)];
}

char *change_ext(const char *path, const char *ext) {
  const char *p = strrchr(path, '/');
  if (p == NULL)
    p = path;

  const char *q = strrchr(p, '.');
  size_t len = q != NULL ? (size_t)(q - path) : strlen(path);
  size_t ext_len = strlen(ext);
  char *s = malloc(len + 1 + ext_len);
  if (s != NULL) {
    memcpy(s, path, len);
    s[len] = '.';
    strcpy(s + (len + 1), ext);
  }
  return s;
}

void put_padding(FILE *fp, uintptr_t start) {
  long cur = ftell(fp);
  if (start > (size_t)cur) {
    size_t size = start - VOIDP2UINT(cur);
    for (size_t i = 0; i < size; ++i)
      fputc(0x00, fp);
  }
}

bool is_file(const char *path) {
  struct stat st;
  return stat(path, &st) == 0 && S_ISREG(st.st_mode);  // Include symbolic link, too.
}

void show_version(const char *exe) {
  if (exe != NULL)
    printf("%s %s\n", exe, VERSION);
  else
    printf("%s\n", VERSION);
}

void error(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  exit(1);
}

void show_error_line(const char *line, const char *p, int len) {
  fprintf(stderr, "%s\n", line);
  size_t pos = p - line;
  if (pos <= strlen(line)) {
    for (size_t i = 0; i < pos; ++i)
      fputc(line[i] == '\t' ? '\t' : ' ', stderr);
    fprintf(stderr, "^");
    for (int i = 1; i < len; ++i)
      fprintf(stderr, "~");
    fprintf(stderr, "\n");
  }
}

bool is_im8(intptr_t x) {
  return x <= ((1LL << 7) - 1) && x >= -(1LL << 7);
}

bool is_im16(intptr_t x) {
  return x <= ((1LL << 15) - 1) && x >= -(1LL << 15);
}

bool is_im32(intptr_t x) {
  return x <= ((1LL << 31) - 1) && x >= -(1LL << 31);
}

const char *skip_whitespaces(const char *s) {
  while (isspace(*s))
    ++s;
  return s;
}

const char *block_comment_start(const char *p) {
  const char *q = skip_whitespaces(p);
  return (*q == '/' && q[1] == '*') ? q : NULL;
}

const char *block_comment_end(const char *p) {
  for (;;) {
    p = strchr(p, '*');
    if (p == NULL)
      return NULL;
    if (*(++p) == '/')
      return p + 1;
  }
}

int64_t wrap_value(int64_t value, int size, bool is_unsigned) {
  if (is_unsigned) {
    switch (size) {
    case 1:  value = (uint8_t)value; break;
    case 2:  value = (uint16_t)value; break;
    case 4:  value = (uint32_t)value; break;
    default:  break;
    }
  } else {
    switch (size) {
    case 1:  value = (int8_t)value; break;
    case 2:  value = (int16_t)value; break;
    case 4:  value = (int32_t)value; break;
    default:  break;
    }
  }
  return value;
}

// Container

Vector *new_vector(void) {
  Vector *vec = malloc_or_die(sizeof(Vector));
  vec_init(vec);
  return vec;
}

void free_vector(Vector *vec) {
  free(vec->data);
  free(vec);
}

void vec_init(Vector *vec) {
  vec->data = NULL;
  vec->capacity = 0;
  vec->len = 0;
}

void vec_clear(Vector *vec) {
  vec->len = 0;
}

void vec_push(Vector *vec, const void *elem) {
  if (vec->capacity <= vec->len) {
    if (vec->capacity <= 0)
      vec->capacity = 16;
    else
      vec->capacity <<= 1;
    vec->data = realloc_or_die(vec->data, sizeof(*vec->data) * vec->capacity);
  }
  vec->data[vec->len++] = (void*)elem;
}

void *vec_pop(Vector *vec) {
  return vec->len > 0 ? vec->data[--vec->len] : NULL;
}

void vec_insert(Vector *vec, int pos, const void *elem) {
  int len = vec->len;
  if (pos < 0 || pos > len)
    return;

  if (pos < len) {
    vec_push(vec, NULL);
    memmove(&vec->data[pos + 1], &vec->data[pos], sizeof(void*) * (len - pos));
    vec->data[pos] = (void*)elem;
  } else {
    vec_push(vec, elem);
  }
}

void vec_remove_at(Vector *vec, int index) {
  if (index < 0 || index >= vec->len)
    return;
  int d = vec->len - index - 1;
  if (d > 0)
    memmove(&vec->data[index], &vec->data[index + 1], d * sizeof(*vec->data));
  --vec->len;
}

bool vec_contains(Vector *vec, void *elem) {
  for (int i = 0, len = vec->len; i < len; ++i) {
    if (vec->data[i] == elem)
      return true;
  }
  return false;
}

// DataStorage

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

void data_align(DataStorage *data, int align) {
  size_t len = data->len;
  size_t aligned_len = ALIGN(len, align);
  size_t add = aligned_len - len;
  if (add <= 0)
    return;

  void *zero = calloc_or_die(add);
  data_append(data, zero, add);
  free(zero);

  assert(data->len == aligned_len);
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

void data_varuint32(DataStorage *data, ssize_t pos, uint64_t val) {
  unsigned char buf[5], *p = buf;
  for (int i = 0; i < 4; ++i) {
    *p++ = (val & 0x7f) | 0x80;
    val >>= 7;
  }
  *p++ = val & 0x7f;
  data_insert(data, pos, buf, p - buf);
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

// StringBuffer

typedef struct {
  const char *start;
  size_t len;
} StringElement;

void sb_init(StringBuffer *sb) {
  sb->elems = new_vector();
}

void sb_clear(StringBuffer *sb) {
  vec_clear(sb->elems);
}

bool sb_empty(StringBuffer *sb) {
  return sb->elems->len == 0;
}

void sb_insert(StringBuffer *sb, int pos, const char *start, const char *end) {
  StringElement *elem = malloc(sizeof(*elem));
  if (elem != NULL) {
    elem->start = start;
    elem->len = end != NULL ? (size_t)(end - start) : strlen(start);
    assert(0 <= pos && pos <= sb->elems->len);
    vec_insert(sb->elems, pos, elem);
  }
}

char *sb_join(StringBuffer *sb, const char *separator) {
  size_t total_len = 0;
  int count = sb->elems->len;
  for (int i = 0; i < count; ++i) {
    StringElement *elem = sb->elems->data[i];
    total_len += elem->len;
  }
  size_t sepalen = separator != NULL ? strlen(separator) : 0;
  if (count > 0 && sepalen > 0)
    total_len += sepalen * (count - 1);

  char *str = malloc(total_len + 1);
  if (str != NULL) {
    char *p = str;
    for (int i = 0; i < count; ++i) {
      if (i > 0 && sepalen > 0) {
        memcpy(p, separator, sepalen);
        p += sepalen;
      }
      StringElement *elem = sb->elems->data[i];
      memcpy(p, elem->start, elem->len);
      p += elem->len;
    }
    *p = '\0';
  }
  return str;
}

// Make sure inline function is out.
extern inline void sb_append(StringBuffer *sb, const char *start, const char *end);
extern inline void sb_prepend(StringBuffer *sb, const char *start, const char *end);
extern inline char *sb_to_string(StringBuffer *sb);

static const char *escape_hex(int c) {
  char *s = malloc_or_die(5);
  snprintf(s, 5, "\\x%02x", c & 0xff);
  return s;
}

static const char *escape(int c) {
  switch (c) {
  case '\0': return "\\0";
  case '\n': return "\\n";
  case '\r': return "\\r";
  case '\t': return "\\t";
  case '"': return "\\\"";
  case '\\': return "\\\\";
  default:
    if (c < 0x20 || c >= 0x7f)
      return escape_hex(c);
    return NULL;
  }
}

void escape_string(const char *str, size_t size, StringBuffer *sb) {
  const char *s, *p;
  const char *end = str + size;
  bool hex = false;
  for (s = p = str; p < end; ++p) {
    const char *e;
    if (hex && isxdigit(*p)) {
      e = escape_hex(*p);
    } else {
      e = escape(*p);
      if (e == NULL) {
        hex = false;
        continue;
      }
    }

    if (p > s)
      sb_append(sb, s, p);
    sb_append(sb, e, NULL);
    s = p + 1;
    hex = e[1] == 'x';
  }
  if (p > s) {
    assert(!hex);
    sb_append(sb, s, p);
  }
}

// Optparse

int optind, optopt;
int opterr = 1;
char *optarg;

int optparse(int argc, char *const argv[], const struct option *opts) {
#define ERROR(...)  do { if (opterr) fprintf(stderr, __VA_ARGS__); } while (0)
  if (optind == 0) {
    optind = 1;
  }

  if (optind >= argc)
    return -1;

  optarg = NULL;
  optopt = 0;

  char *arg = argv[optind];
  char *p = arg;
  if (*p != '-')
    return -1;

  p += 1;
  ++optind;
  for (; opts->name != NULL; ++opts) {
    size_t len = strlen(opts->name);
    if (strncmp(p, opts->name, len) == 0) {
      int opt = opts->val;
      if (opt == 0)
        opt = arg[1];
      char *q = p + len;
      char c = *q;
      if (opts->has_arg) {
        if (c != '\0') {
          optarg = q + (c == '=' ? 1 : 0);
        } else if (opts->has_arg == required_argument) {
          if (optind < argc) {
            optarg = argv[optind++];
          } else {
            ERROR("%s: option '--%s' requires an argument\n", argv[0], opts->name);
            break;
          }
        }
      } else {
        if (c != '\0') {
          if (c != '=')
            continue;
          ERROR("%s: option '--%s' doesn't allow an argument\n", argv[0], opts->name);
          break;
        }
      }
      return opt;
    }
  }

  optopt = arg[1];
  return '?';
#undef ERROR
}
