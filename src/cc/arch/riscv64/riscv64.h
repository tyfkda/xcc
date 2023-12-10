#pragma once

#include <stdbool.h>

#include "emit_util.h"

#ifndef IM
#define IM(x)  im(x)
#endif
#ifndef NUM
#define NUM(x)  num(x)
#endif
#ifndef HEXNUM
#define HEXNUM(x)  hexnum(x)
#endif
#ifndef FLONUM
#define FLONUM(x)  flonum(x)
#endif
#ifndef MANGLE
#define MANGLE(label)  mangle(label)
#endif

#define _UXTW(shift)  fmt("uxtw #%d", shift)
#define _LSL(shift)   fmt("lsl #%d", shift)

#define A0    "a0"
#define A1    "a1"
#define A2    "a2"
#define A3    "a3"
#define A4    "a4"
#define A5    "a5"
#define A6    "a6"
#define A7    "a7"
#define S0    "s0"
#define S1    "s1"
#define S2    "s2"
#define S3    "s3"
#define S4    "s4"
#define S5    "s5"
#define S6    "s6"
#define S7    "s7"
#define S8    "s8"
#define S9    "s9"
#define S10   "s10"
#define S11   "s11"
#define T0    "t0"
#define T1    "t1"
#define T2    "t2"

// Condition
#define CEQ  "eq"
#define CNE  "ne"
#define CLT  "lt"
#define CGT  "gt"
#define CLE  "le"
#define CGE  "ge"
#define CLO  "lo"
#define CHI  "hi"
#define CLS  "ls"
#define CHS  "hs"

#define LI(o1, o2)            EMIT_ASM("li", o1, o2)
#define RET()                 EMIT_ASM("ret")

#define _BYTE(x)       EMIT_ASM(".byte", x)
#define _WORD(x)       EMIT_ASM(".short", x)  // Or .hword
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

#ifdef __APPLE__
#define _RODATA()      _SECTION("__DATA,__const")
#define _LOCAL(x)      ((void)0)
#else
#define _RODATA()      _SECTION(".rodata")
#define _LOCAL(x)      EMIT_ASM(".local", x)
#endif

#define _BSS(label, size, align)  emit_bss(label, size, align)


void mov_immediate(const char *dst, int64_t value, bool is_unsigned);
