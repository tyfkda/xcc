#include "../../config.h"
#include "emit_util.h"

#include <assert.h>
#include <inttypes.h>  // PRId64
#include <stdarg.h>
#include <stdint.h>  // int64_t
#include <stdlib.h>  // realloc

#include "ast.h"
#include "cc_misc.h"
#include "ir.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

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
    p->buf = realloc_or_die(p->buf, newsize);
    p->size = newsize;

    // Retry
    va_start(ap, fm);
    int n2 = vsnprintf(p->buf, p->size, fm, ap);
    va_end(ap);
    assert(n2 == n && n2 < p->size);
    UNUSED(n2);
  }
  return p->buf;
#undef MIN_SIZE
#undef N
}

char *fmt_name(const Name *name) {
  return fmt("%.*s", NAMES(name));
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
  fprintf(emit_fp, "/* ");
  vfprintf(emit_fp, comment, ap);
  fprintf(emit_fp, " */\n");
  va_end(ap);
}

void emit_align_p2(int align) {
  if (align <= 1)
    return;
  assert(IS_POWER_OF_2(align));
  fprintf(emit_fp, "\t.p2align %d\n", most_significant_bit(align));
}

void emit_bss(const char *label, size_t size, size_t align) {
#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
  fprintf(emit_fp, "\t.zerofill __DATA,__bss,%s,%zu,%d\n", label, size,
          most_significant_bit(align));
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

bool function_not_returned(FuncBackend *fnbe) {
  BB *bb = fnbe->bbcon->bbs->data[fnbe->bbcon->bbs->len - 1];
  if (bb->irs->len > 0) {
    IR *ir = bb->irs->data[bb->irs->len - 1];
    if (ir->kind == IR_JMP && ir->jmp.cond == COND_ANY && ir->jmp.bb != NULL) {
      // No fallthrough exists: the function does not return.
      return true;
    }
  }
  return false;
}

static void emit_align(void *ud, int align) {
  UNUSED(ud);
  EMIT_ALIGN(align);
}
static void emit_number(void *ud, const Type *type, Expr *var, int64_t offset) {
  UNUSED(ud);
  const char *output;
  if (var == NULL) {
    output = is_flonum(type) ? hexnum(offset) : num(offset);
  } else {
    const Name *name = var->var.name;
    Scope *scope;
    VarInfo *varinfo = scope_find(var->var.scope, name, &scope);
    assert(varinfo != NULL);
    if (!is_global_scope(scope) && varinfo->storage & VS_STATIC) {
      varinfo = varinfo->static_.gvar;
      assert(varinfo != NULL);
      name = varinfo->name;
    }

    char *label = fmt_name(name);
    if ((varinfo->storage & VS_STATIC) == 0)
      label = mangle(label);
    label = quote_label(label);
    if (offset == 0) {
      output = label;
    } else {
      output = fmt("%s + %" PRId64, label, offset);
    }
  }

  switch (type_size(type)) {
  case 1: _BYTE(output); break;
  case 2: _SHORT(output); break;
  case 4: _LONG(output); break;
  case 8: _QUAD(output); break;
  default: assert(false); break;
  }
}
static void emit_string(void *ud, Expr *str, size_t size) {
  UNUSED(ud);
  assert(str->kind == EX_STR);
  size_t src_size = str->str.len * type_size(str->type->pa.ptrof);
  if (src_size > size)
    src_size = size;

  StringBuffer sb;
  sb_init(&sb);
  sb_append(&sb, "\"", NULL);
  escape_string(str->str.buf, src_size, &sb);
  if (size > src_size) {
    static const char NULCHR[] = "\\0";
    for (size_t i = 0, n = size - src_size; i < n; ++i)
      sb_append(&sb, NULCHR, NULL);
  }
  sb_append(&sb, "\"", NULL);
  const char *escaped = sb_to_string(&sb);
  _ASCII(escaped);
}

void emit_varinfo(const VarInfo *varinfo, const Initializer *init) {
  static const ConstructInitialValueVTable kVtable = {
    .emit_align = emit_align,
    .emit_number = emit_number,
    .emit_string = emit_string,
  };

  const Name *name = varinfo->name;
  if (init != NULL) {
    if (varinfo->type->qualifier & TQ_CONST)
      _RODATA();
    else
      _DATA();
  }

  char *label = fmt_name(name);
  if ((varinfo->storage & VS_STATIC) == 0) {  // global
    label = quote_label(MANGLE(label));
    _GLOBL(label);
  } else {
    label = quote_label(label);
    _LOCAL(label);
  }

  if (init != NULL) {
    EMIT_ALIGN(align_size(varinfo->type));
    EMIT_LABEL(label);
    construct_initial_value(varinfo->type, init, &kVtable, NULL);
  } else {
    size_t size = type_size(varinfo->type);
    if (size < 1)
      size = 1;

    size_t align = align_size(varinfo->type);
    _BSS(label, size, align);
  }
}
