#pragma once

#include <stdio.h>

void init_emit(FILE *fp);
void emit_label(const char *label);
void emit_asm2(const char *op, const char *operand1, const char *operand2);
void emit_align(int align);
void emit_comment(const char *comment, ...);
