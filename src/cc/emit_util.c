#include "../config.h"
#include "emit_util.h"

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

char *quote_label(char *label) {
  for (const unsigned char *p = (unsigned char*)label; *p != '\0'; ++p) {
    if (isutf8first(*p))
      return fmt("\"%s\"", label);
  }
  return label;
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

char *mangle(char *label) {
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

void emit_asm3(const char *op, const char *operand1, const char *operand2, const char *operand3) {
  if (operand1 == NULL) {
    fprintf(emit_fp, "\t%s\n", op);
  } else if (operand2 == NULL) {
    fprintf(emit_fp, "\t%s %s\n", op, operand1);
  } else if (operand3 == NULL) {
    fprintf(emit_fp, "\t%s %s, %s\n", op, operand1, operand2);
  } else {
    fprintf(emit_fp, "\t%s %s, %s, %s\n", op, operand1, operand2, operand3);
  }
}

void emit_asm4(const char *op, const char *operand1, const char *operand2, const char *operand3, const char *operand4) {
  if (operand1 == NULL) {
    fprintf(emit_fp, "\t%s\n", op);
  } else if (operand2 == NULL) {
    fprintf(emit_fp, "\t%s %s\n", op, operand1);
  } else if (operand3 == NULL) {
    fprintf(emit_fp, "\t%s %s, %s\n", op, operand1, operand2);
  } else if (operand4 == NULL) {
    fprintf(emit_fp, "\t%s %s, %s, %s\n", op, operand1, operand2, operand3);
  } else {
    fprintf(emit_fp, "\t%s %s, %s, %s, %s\n", op, operand1, operand2, operand3, operand4);
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
