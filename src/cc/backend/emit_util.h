// Code emission

#pragma once

#include <stdbool.h>
#include <stdint.h>  // int64_t
#include <stdio.h>

typedef struct FuncBackend FuncBackend;
typedef struct Initializer Initializer;
typedef struct VarInfo VarInfo;

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

bool function_not_returned(FuncBackend *fnbe);

#define _BYTE(x)       EMIT_ASM(".byte", x)
#define _SHORT(x)      EMIT_ASM(".short", x)  // Or .hword
#define _LONG(x)       EMIT_ASM(".long", x)
#define _QUAD(x)       EMIT_ASM(".quad", x)
#define _FLOAT(x)      EMIT_ASM(".float", x)
#define _DOUBLE(x)     EMIT_ASM(".double", x)
#define _GLOBL(x)      EMIT_ASM(".globl", x)
#define _COMM(x, y)    EMIT_ASM(".comm", x, y)
#define _ASCII(x)      EMIT_ASM(".ascii", x)
#define _SECTION(x)    EMIT_ASM(".section", x)
#define _TEXT()        EMIT_ASM(".text")
#define _DATA()        EMIT_ASM(".data")

#define EMIT_ALIGN(x)  emit_align_p2(x)

#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
#define _RODATA()      _SECTION("__DATA,__const")
#define _LOCAL(x)      ((void)0)
#else
#define _RODATA()      _SECTION(".rodata")
#define _LOCAL(x)      EMIT_ASM(".local", x)
#endif

#define _BSS(label, size, align)  emit_bss(label, size, align)

#ifndef MANGLE
#define MANGLE(label)  mangle(label)
#endif

void emit_varinfo(const VarInfo *varinfo, const Initializer *init);
