// Code emission

#pragma once

#include <stdint.h>  // intptr_t
#include <stdio.h>

typedef struct Name Name;

char *fmt(const char *s, ...);
char *fmt_name(const Name *name);
char *quote_label(char *label);
char *num(intptr_t x);  // x
char *hexnum(intptr_t x);  // 0xnn
#ifndef __NO_FLONUM
char *flonum(double x);
#endif
char *im(intptr_t x);  // $x
char *immediate_offset(const char *reg, int offset);
char *pre_index(const char *reg, int offset);
char *post_index(const char *reg, int offset);
char *reg_offset(const char *base, const char *reg, const char *shift);
char *label_at_page(char *label);
char *label_at_pageoff(char *label);
char *mangle(char *label);

void init_emit(FILE *fp);
void emit_label(const char *label);
void emit_asm2(const char *op, const char *operand1, const char *operand2);
void emit_asm3(const char *op, const char *operand1, const char *operand2, const char *operand3);
void emit_asm4(const char *op, const char *operand1, const char *operand2, const char *operand3, const char *operand4);
void emit_align(int align);
void emit_align_p2(int align);
void emit_comment(const char *comment, ...);
