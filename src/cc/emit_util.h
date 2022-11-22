// Code emission

#pragma once

#include <stdint.h>  // int64_t
#include <stdio.h>

#ifndef EMIT_LABEL
#define EMIT_LABEL(label)  emit_label(label)
#endif

#ifndef EMIT_ASM
#define EMIT_ASM(...)  _SELECT_EMIT_ASM(__VA_ARGS__, 4, 3, 2, 1, 0, -1)(__VA_ARGS__)
#define _SELECT_EMIT_ASM(_4, _3, _2, _1, _0, count, ...)  emit_asm ## count
#endif

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
void emit_asm0(const char *op);
void emit_asm1(const char *op, const char *a1);
void emit_asm2(const char *op, const char *a1, const char *a2);
void emit_asm3(const char *op, const char *a1, const char *a2, const char *a3);
void emit_asm4(const char *op, const char *a1, const char *a2, const char *a3, const char *a4);
void emit_align_p2(int align);
void emit_comment(const char *comment, ...);
void emit_bss(const char *label, size_t size, size_t align);
