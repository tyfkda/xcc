#include "emit.h"

#include <assert.h>
#include <inttypes.h>  // PRIdPTR
#include <stdarg.h>
#include <stdint.h>  // intptr_t

#include "table.h"
#include "util.h"

#ifdef __APPLE__
#define MANGLE_PREFIX  "_"
#endif

static FILE *emit_fp;

char *fmt(const char *s, ...) {
  static char buf[4][64];
  static int index;
  char *p = buf[index];
  if (++index >= 4)
    index = 0;
  va_list ap;
  va_start(ap, s);
  vsnprintf(p, sizeof(buf[0]), s, ap);
  va_end(ap);
  return p;
}

char *fmt_name(const Name *name) {
  return fmt("%.*s", name->bytes, name->chars);
}

char *num(intptr_t x) {
  return fmt("%" PRIdPTR, x);
}

char *hexnum(intptr_t x) {
  return fmt("0x%" PRIxPTR, x);
}

#ifndef __NO_FLONUM
char *flonum(double x) {
  return fmt("%.16g", x);
}
#endif

char *im(intptr_t x) {
  return fmt("$%" PRIdPTR, x);
}

char *indirect(const char *base, const char *index, int scale) {
  if (index == NULL) {
    return fmt("(%s)", base);
  } else {
    if (scale == 1)
      return fmt("(%s,%s)", base, index);
    else
      return fmt("(%s,%s,%d)", base, index, scale);
  }
}

char *offset_indirect(int offset, const char *base, const char *index, int scale) {
  if (offset == 0)
    return indirect(base, index, scale);

  if (index == NULL) {
    return fmt("%d(%s)", offset, base);
  } else {
    if (scale == 1)
      return fmt("%d(%s,%s)", offset, base, index);
    else
      return fmt("%d(%s,%s,%d)", offset, base, index, scale);
  }
}

char *label_indirect(const char *label, const char *reg) {
  return fmt("%s(%s)", label, reg);
}

const char *mangle(const char *label) {
#ifdef MANGLE_PREFIX
  return fmt(MANGLE_PREFIX "%s", label);
#else
  return label;
#endif
}

void emit_asm2(const char *op, const char *operand1, const char *operand2) {
  if (operand1 == NULL) {
    fprintf(emit_fp, "\t%s\n", op);
  } else if (operand2 == NULL) {
    fprintf(emit_fp, "\t%s %s\n", op, operand1);
  } else {
    fprintf(emit_fp, "\t%s %s, %s\n", op, operand1, operand2);
  }
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

void emit_align(int align) {
  if (align <= 1)
    return;
  fprintf(emit_fp, "\t.align %d\n", align);
}

void emit_align_p2(int align) {
  if (align <= 1)
    return;

  // On Apple platform,
  // .align directive is actually .p2align,
  // so it has to find power of 2.
  assert(IS_POWER_OF_2(align));
  int bit, x = align;
  for (bit = 0;; ++bit) {
    x >>= 1;
    if (x <= 0)
      break;
  }
  fprintf(emit_fp, "\t.p2align %d\n", bit);
}

void init_emit(FILE *fp) {
  emit_fp = fp;
}
