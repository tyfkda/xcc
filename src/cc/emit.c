#include "emit.h"

#include <inttypes.h>  // PRIdPTR
#include <stdarg.h>
#include <stdint.h>  // intptr_t

#include "x86_64.h"

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

char *num(intptr_t x) {
  return fmt("%"PRIdPTR, x);
}

char *im(intptr_t x) {
  return fmt("$%"PRIdPTR, x);
}

char *indirect(const char *reg) {
  return fmt("(%s)", reg);
}

char *offset_indirect(int offset, const char *reg) {
  return fmt("%d(%s)", offset, reg);
}

char *label_indirect(const char *label, const char *reg) {
  return fmt("%s(%s)", label, reg);
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
  if ((align) > 1)
    _ALIGN(NUM(align));
}

void init_emit(FILE *fp) {
  emit_fp = fp;
}
