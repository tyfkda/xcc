#include "../config.h"
#include "emit_util.h"

#include <assert.h>
#include <inttypes.h>  // PRId64
#include <stdarg.h>
#include <stdint.h>  // int64_t
#include <stdlib.h>  // realloc

#include "table.h"
#include "util.h"

#ifdef __APPLE__
#define MANGLE_PREFIX  "_"
#endif

static FILE *emit_fp;

char *fmt(const char *fm, ...) {
#define N  8
#define MIN_SIZE  16
  typedef struct {
    char *buf;
    int size;
  } Buf;
  static Buf ringbuf[N];
  static int index;
  Buf *p = &ringbuf[index];
  if (++index >= N)
    index = 0;

  va_list ap;
  va_start(ap, fm);
  int n = vsnprintf(p->buf, p->size, fm, ap);
  va_end(ap);

  if (n >= p->size) {
    int newsize = n + (n >> 1);
    if (newsize < MIN_SIZE)
      newsize = MIN_SIZE;
    char *newbuf = realloc(p->buf, newsize);
    if (newbuf == NULL)
      error("Out of memory");
    p->buf = newbuf;
    p->size = newsize;

    // Retry
    va_start(ap, fm);
    int n2 = vsnprintf(p->buf, p->size, fm, ap);
    va_end(ap);
    assert(n2 == n && n2 < p->size);
  }
  return p->buf;
#undef MIN_SIZE
#undef N
}

char *fmt_name(const Name *name) {
  return fmt("%.*s", name->bytes, name->chars);
}

char *quote_label(char *label) {
  for (const unsigned char *p = (unsigned char*)label; *p != '\0'; ++p) {
    if (isutf8first(*p))
      return fmt("\"%s\"", label);
  }
  return label;
}

char *num(int64_t x) {
  return fmt("%" PRId64, x);
}

char *hexnum(int64_t x) {
  return fmt("0x%" PRIxPTR, x);
}

#ifndef __NO_FLONUM
char *flonum(double x) {
  return fmt("%.16g", x);
}
#endif

char *mangle(char *label) {
#ifdef MANGLE_PREFIX
  return fmt(MANGLE_PREFIX "%s", label);
#else
  return label;
#endif
}

void emit_asm0(const char *op) {
  fprintf(emit_fp, "\t%s\n", op);
}

void emit_asm1(const char *op, const char *a1) {
  fprintf(emit_fp, "\t%s %s\n", op, a1);
}

void emit_asm2(const char *op, const char *a1, const char *a2) {
  fprintf(emit_fp, "\t%s %s, %s\n", op, a1, a2);
}

void emit_asm3(const char *op, const char *a1, const char *a2, const char *a3) {
  fprintf(emit_fp, "\t%s %s, %s, %s\n", op, a1, a2, a3);
}

void emit_asm4(const char *op, const char *a1, const char *a2, const char *a3, const char *a4) {
  fprintf(emit_fp, "\t%s %s, %s, %s, %s\n", op, a1, a2, a3, a4);
}

void emit_label(const char *label) {
  fprintf(emit_fp, "%s:\n", label);
}

void emit_comment(const char *comment, ...) {
  if (comment == NULL) {
    fprintf(emit_fp, "\n");
    return;
  }

  va_list ap;
  va_start(ap, comment);
  fprintf(emit_fp, "// ");
  vfprintf(emit_fp, comment, ap);
  fprintf(emit_fp, "\n");
  va_end(ap);
}

void emit_align_p2(int align) {
  if (align <= 1)
    return;
  assert(IS_POWER_OF_2(align));
  fprintf(emit_fp, "\t.p2align %d\n", most_significant_bit(align));
}

void emit_bss(const char *label, size_t size, size_t align) {
#ifdef __APPLE__
  fprintf(emit_fp, "\t.zerofill __DATA,__bss,%s,%zu,%d\n", label, size, most_significant_bit(align));
#else
  if (align <= 1)
    emit_asm2(".comm", label, num(size));
  else
    fprintf(emit_fp, "\t.comm %s, %zu, %zu\n", label, size, align);
#endif
}

void init_emit(FILE *fp) {
  emit_fp = fp;
}
