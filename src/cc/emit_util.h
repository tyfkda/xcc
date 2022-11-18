// Code emission

#pragma once

#include <stdint.h>  // int64_t
#include <stdio.h>

typedef struct Name Name;

char *fmt(const char *s, ...);
char *fmt_name(const Name *name);
char *quote_label(char *label);
char *num(int64_t x);  // x
char *hexnum(int64_t x);  // 0xnn
#ifndef __NO_FLONUM
char *flonum(double x);
#endif
char *mangle(char *label);

void init_emit(FILE *fp);
void emit_label(const char *label);
void emit_asm2(const char *op, const char *operand1, const char *operand2);
void emit_asm3(const char *op, const char *operand1, const char *operand2, const char *operand3);
void emit_asm4(const char *op, const char *operand1, const char *operand2, const char *operand3, const char *operand4);
void emit_align_p2(int align);
void emit_comment(const char *comment, ...);
void emit_bss(const char *label, size_t size, size_t align);
