// Code emission

#pragma once

#include <stdint.h>  // intptr_t
#include <stdio.h>

typedef struct Name Name;

char *fmt(const char *s, ...);
char *fmt_name(const Name *name);
char *num(intptr_t x);  // x
char *hexnum(intptr_t x);  // 0xnn
#ifndef __NO_FLONUM
char *flonum(double x);
#endif
char *im(intptr_t x);  // $x
char *indirect(const char *base, const char *index, int scale);
char *offset_indirect(int offset, const char *base, const char *index, int scale);
char *label_indirect(const char *label, const char *reg);
const char *mangle(const char *label);

void init_emit(FILE *fp);
void emit_label(const char *label);
void emit_asm2(const char *op, const char *operand1, const char *operand2);
void emit_align(int align);
void emit_align_p2(int align);
void emit_comment(const char *comment, ...);
