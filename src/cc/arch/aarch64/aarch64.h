#pragma once

#include "emit.h"

#ifndef EMIT_LABEL
#define EMIT_LABEL(label)  emit_label(label)
#endif
#ifndef EMIT_ASM0
#define EMIT_ASM0(op)  EMIT_ASM2(op, NULL, NULL)
#endif
#ifndef EMIT_ASM1
#define EMIT_ASM1(op, operand1)  EMIT_ASM2(op, operand1, NULL)
#endif
#ifndef EMIT_ASM2
#define EMIT_ASM2(op, operand1, operand2)  EMIT_ASM3(op, operand1, operand2, NULL)
#endif
#ifndef EMIT_ASM3
#define EMIT_ASM3(op, operand1, operand2, operand3)  emit_asm3(op, operand1, operand2, operand3)
#endif
#ifndef EMIT_ASM4
#define EMIT_ASM4(op, operand1, operand2, operand3, operand4)  emit_asm4(op, operand1, operand2, operand3, operand4)
#endif

#ifndef IM
#define IM(x)  im(x)
#endif
#ifndef MANGLE
#define MANGLE(label)  mangle(label)
#endif

#define _LSL(shift)   fmt("lsl #%d", shift)

#define W0    "w0"
#define W1    "w1"
#define W2    "w2"
#define W3    "w3"
#define W4    "w4"
#define W5    "w5"
#define W6    "w6"
#define W7    "w7"
#define W8    "w8"
#define W9    "w9"
#define W10   "w10"
#define W11   "w11"
#define W12   "w12"
#define W13   "w13"
#define W14   "w14"
#define W15   "w15"
#define W16   "w16"
#define W17   "w17"
#define W18   "w18"
#define W19   "w19"
#define W20   "w20"
#define W21   "w21"
#define W22   "w22"
#define W23   "w23"
#define W24   "w24"
#define W25   "w25"
#define W26   "w26"
#define W27   "w27"
#define W28   "w28"
// #define W29   "w29"
// #define W30   "w30"
// #define W31   "w31"

#define X0    "x0"
#define X1    "x1"
#define X2    "x2"
#define X3    "x3"
#define X4    "x4"
#define X5    "x5"
#define X6    "x6"
#define X7    "x7"
#define X8    "x8"
#define X9    "x9"
#define X10   "x10"
#define X11   "x11"
#define X12   "x12"
#define X13   "x13"
#define X14   "x14"
#define X15   "x15"
#define X16   "x16"
#define X17   "x17"
#define X18   "x18"
#define X19   "x19"
#define X20   "x20"
#define X21   "x21"
#define X22   "x22"
#define X23   "x23"
#define X24   "x24"
#define X25   "x25"
#define X26   "x26"
#define X27   "x27"
#define X28   "x28"
// #define X29   "x29"
// #define X30   "x30"
// #define X31   "x31"
#define FP    "fp"  // x29
#define LR    "lr"  // x30
#define SP    "sp"  // x31

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

#define MOV(o1, o2)           EMIT_ASM2("mov", o1, o2)
#define MOVK(o1, o2, o3)      EMIT_ASM3("movk", o1, o2, o3)
#define ADD(o1, o2, o3)       EMIT_ASM3("add", o1, o2, o3)
#define SUB(o1, o2, o3)       EMIT_ASM3("sub", o1, o2, o3)
#define MUL(o1, o2, o3)       EMIT_ASM3("mul", o1, o2, o3)
#define SDIV(o1, o2, o3)      EMIT_ASM3("sdiv", o1, o2, o3)
#define UDIV(o1, o2, o3)      EMIT_ASM3("udiv", o1, o2, o3)
#define MSUB(o1, o2, o3, o4)  EMIT_ASM4("msub", o1, o2, o3, o4)
#define CMP(o1, o2)           EMIT_ASM2("cmp", o1, o2)
#define BRANCH(o1)            EMIT_ASM1("b", o1)
#define Bcc(c, o1)            EMIT_ASM1("b" c, o1)
#define RET()                 EMIT_ASM0("ret")

#define _BYTE(x)       EMIT_ASM1(".byte", x)
#define _WORD(x)       EMIT_ASM1(".word", x)
#define _LONG(x)       EMIT_ASM1(".long", x)
#define _QUAD(x)       EMIT_ASM1(".quad", x)
#define _FLOAT(x)      EMIT_ASM1(".float", x)
#define _DOUBLE(x)     EMIT_ASM1(".double", x)
#define _GLOBL(x)      EMIT_ASM1(".globl", x)
#define _COMM(x, y)    EMIT_ASM2(".comm", x, y)
#define _ASCII(x)      EMIT_ASM1(".ascii", x)
#define _SECTION(x)    EMIT_ASM1(".section", x)
#define _TEXT()        EMIT_ASM0(".text")
#define _DATA()        EMIT_ASM0(".data")

#ifdef __APPLE__
#define _RODATA()      _SECTION("__DATA,__const")
#define EMIT_ALIGN(x)  emit_align_p2(x)
#define _LOCAL(x)      (0)
#else
#define _RODATA()      _SECTION(".rodata")
#define EMIT_ALIGN(x)  emit_align(x)
#define _LOCAL(x)      EMIT_ASM1(".local", x)
#endif
