// Code emission

#pragma once

#include <stdint.h>  // intptr_t
#include <stdio.h>

char *fmt(const char *s, ...);
char *num(intptr_t x);  // x
char *im(intptr_t x);  // $x
char *indirect(const char *reg);
char *offset_indirect(int offset, const char *reg);
char *label_indirect(const char *label, const char *reg);

void init_emit(FILE *fp);
void emit_label(const char *label);
void emit_asm2(const char *op, const char *operand1, const char *operand2);
void emit_align(int align);
void emit_comment(const char *comment, ...);
