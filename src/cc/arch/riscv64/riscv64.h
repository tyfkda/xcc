#pragma once

#include <stdbool.h>

#include "emit_util.h"

#ifndef IM
#define IM(x)  im(x)
#endif
#ifndef IMMEDIATE_OFFSET
#define IMMEDIATE_OFFSET(ofs, reg)  immediate_offset(ofs, reg)
#endif
#ifndef IMMEDIATE_OFFSET0
#define IMMEDIATE_OFFSET0(reg)  immediate_offset(0, reg)
#endif
#ifndef LABEL_OFFSET_HI
#define LABEL_OFFSET_HI(label)  label_offset_hi(label)
#endif
#ifndef LABEL_OFFSET_LO
#define LABEL_OFFSET_LO(label)  label_offset_lo(label)
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

#define ZERO  "zero"  // x0: Zero register
#define RA    "ra"    // x1: Return Address
#define SP    "sp"    // x2: Stack Pointer
#define FP    "fp"    // x8: Frame Pointer
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
#define CEQ   "eq"
#define CNE   "ne"
#define CLT   "lt"
#define CGE   "ge"
#define CLTU  "ltu"
#define CGEU  "geu"

#define LI(o1, o2)            EMIT_ASM("li", o1, o2)
#define LUI(o1, o2)           EMIT_ASM("lui", o1, o2)
#define ADD(o1, o2, o3)       EMIT_ASM("add", o1, o2, o3)
#define ADDI(o1, o2, o3)      EMIT_ASM("addi", o1, o2, o3)
#define ADDW(o1, o2, o3)      EMIT_ASM("addw", o1, o2, o3)
#define ADDIW(o1, o2, o3)     EMIT_ASM("addiw", o1, o2, o3)
#define SUB(o1, o2, o3)       EMIT_ASM("sub", o1, o2, o3)
#define SUBW(o1, o2, o3)      EMIT_ASM("subw", o1, o2, o3)
#define MUL(o1, o2, o3)       EMIT_ASM("mul", o1, o2, o3)
#define MULW(o1, o2, o3)      EMIT_ASM("mulw", o1, o2, o3)
#define DIV(o1, o2, o3)       EMIT_ASM("div", o1, o2, o3)
#define DIVU(o1, o2, o3)      EMIT_ASM("divu", o1, o2, o3)
#define DIVW(o1, o2, o3)      EMIT_ASM("divw", o1, o2, o3)
#define DIVUW(o1, o2, o3)     EMIT_ASM("divuw", o1, o2, o3)
#define REM(o1, o2, o3)       EMIT_ASM("rem", o1, o2, o3)
#define REMU(o1, o2, o3)      EMIT_ASM("remu", o1, o2, o3)
#define REMW(o1, o2, o3)      EMIT_ASM("remw", o1, o2, o3)
#define REMUW(o1, o2, o3)     EMIT_ASM("remuw", o1, o2, o3)
#define AND(o1, o2, o3)       EMIT_ASM("and", o1, o2, o3)
#define ANDI(o1, o2, o3)      EMIT_ASM("andi", o1, o2, o3)
#define OR(o1, o2, o3)        EMIT_ASM("or", o1, o2, o3)
#define ORI(o1, o2, o3)       EMIT_ASM("ori", o1, o2, o3)
#define XOR(o1, o2, o3)       EMIT_ASM("xor", o1, o2, o3)
#define XORI(o1, o2, o3)      EMIT_ASM("xori", o1, o2, o3)
#define SLL(o1, o2, o3)       EMIT_ASM("sll", o1, o2, o3)    // Logical left shift
#define SLLI(o1, o2, o3)      EMIT_ASM("slli", o1, o2, o3)   // Logical left shift
#define SLLIW(o1, o2, o3)     EMIT_ASM("slliw", o1, o2, o3)  // Logical left shift, 32bit
#define SRL(o1, o2, o3)       EMIT_ASM("srl", o1, o2, o3)    // Logical right shift
#define SRLI(o1, o2, o3)      EMIT_ASM("srli", o1, o2, o3)   // Logical right shift
#define SRA(o1, o2, o3)       EMIT_ASM("sra", o1, o2, o3)    // Arithmetic right shift
#define SRAI(o1, o2, o3)      EMIT_ASM("srai", o1, o2, o3)   // Arithmetic right shift
#define J(o1)                 EMIT_ASM("j", o1)              // => jal zero, o1
#define JR(o1)                EMIT_ASM("jr", o1)             // => jalr zero, 0(o1)
#define JALR(o1)              EMIT_ASM("jalr", o1)           // => jalr ra, 0(o1)
#define Bcc(c, o1, o2, o3)    EMIT_ASM("b" c, o1, o2, o3)
#define CALL(o1)              EMIT_ASM("call", o1)
#define RET()                 EMIT_ASM("ret")

#define LB(o1, o2)            EMIT_ASM("lb", o1, o2)
#define LH(o1, o2)            EMIT_ASM("lh", o1, o2)
#define LW(o1, o2)            EMIT_ASM("lw", o1, o2)
#define LD(o1, o2)            EMIT_ASM("ld", o1, o2)
#define LBU(o1, o2)           EMIT_ASM("lbu", o1, o2)
#define LHU(o1, o2)           EMIT_ASM("lhu", o1, o2)
#define LWU(o1, o2)           EMIT_ASM("lwu", o1, o2)
#define SB(o1, o2)            EMIT_ASM("sb", o1, o2)
#define SH(o1, o2)            EMIT_ASM("sh", o1, o2)
#define SW(o1, o2)            EMIT_ASM("sw", o1, o2)
#define SD(o1, o2)            EMIT_ASM("sd", o1, o2)

#define MV(o1, o2)            EMIT_ASM("mv", o1, o2)         // => addi o1, o2, 0
#define NEG(o1, o2)           EMIT_ASM("neg", o1, o2)        // => sub o1, zero, o2
#define NOT(o1, o2)           EMIT_ASM("not", o1, o2)        // => xori o1, o2, -1
#define SEXTW(o1, o2)         EMIT_ASM("sext.w", o1, o2)     // => addiw o1, o2, 0

#define SEQZ(o1, o2)          EMIT_ASM("seqz", o1, o2)
#define SNEZ(o1, o2)          EMIT_ASM("snez", o1, o2)
#define SLTZ(o1, o2)          EMIT_ASM("sltz", o1, o2)
#define SGTZ(o1, o2)          EMIT_ASM("sgtz", o1, o2)
#define SLT(o1, o2, o3)       EMIT_ASM("slt", o1, o2, o3)
#define SLTI(o1, o2, o3)      EMIT_ASM("slti", o1, o2, o3)
#define SLTU(o1, o2, o3)      EMIT_ASM("sltu", o1, o2, o3)
#define SLTIU(o1, o2, o3)     EMIT_ASM("sltiu", o1, o2, o3)

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

#define _RODATA()      _SECTION(".rodata")
#define _LOCAL(x)      EMIT_ASM(".local", x)

#define _BSS(label, size, align)  emit_bss(label, size, align)


void mov_immediate(const char *dst, int64_t value, bool is_unsigned);
