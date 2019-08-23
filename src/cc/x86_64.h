#pragma once

#include <inttypes.h>  // PRIdPTR

#ifndef ADD_LABEL
#define ADD_LABEL(label)  add_asm_label(label)
#endif
#ifndef ADD_ASM0
#define ADD_ASM0(op)  ADD_ASM2(op, NULL, NULL)
#endif
#ifndef ADD_ASM1
#define ADD_ASM1(op, operand1)  ADD_ASM2(op, operand1, NULL)
#endif
#ifndef ADD_ASM2
#define ADD_ASM2(op, operand1, operand2)  add_asm2(op, operand1, operand2)
#endif

#ifndef IM
#define IM(x)  im(x)
#endif
#ifndef INDIRECT
#define INDIRECT(x)  indirect(x)
#endif
#ifndef OFFSET_INDIRECT
#define OFFSET_INDIRECT(ofs, x)  offset_indirect(ofs, x)
#endif
#ifndef LABEL_INDIRECT
#define LABEL_INDIRECT(label, x)  label_indirect(label, x)
#endif
#ifndef NUM
#define NUM(x)  num(x)
#endif

#define AL     "%al"
#define CL     "%cl"
#define DL     "%dl"
#define BL     "%bl"
#define SPL    "%spl"
#define BPL    "%bpl"
#define SIL    "%sil"
#define DIL    "%dil"
#define R8B    "%r8b"
#define R9B    "%r9b"

#define AX     "%ax"
#define CX     "%cx"
#define DX     "%dx"
#define BX     "%bx"
#define SP     "%sp"
#define BP     "%bp"
#define SI     "%si"
#define DI     "%di"
#define R8W    "%r8w"
#define R9W    "%r9w"

#define EAX    "%eax"
#define ECX    "%ecx"
#define EDX    "%edx"
#define EBX    "%ebx"
#define ESP    "%esp"
#define EBP    "%ebp"
#define ESI    "%esi"
#define EDI    "%edi"
#define R8D    "%r8d"
#define R9D    "%r9d"

#define RAX    "%rax"
#define RCX    "%rcx"
#define RDX    "%rdx"
#define RBX    "%rbx"
#define RSP    "%rsp"
#define RBP    "%rbp"
#define RSI    "%rsi"
#define RDI    "%rdi"
#define R8     "%r8"
#define R9     "%r9"

#define RIP    "%rip"

#define MOV(o1, o2)    ADD_ASM2("mov", o1, o2)
#define MOVSX(o1, o2)  ADD_ASM2("movsx", o1, o2)
#define LEA(o1, o2)    ADD_ASM2("lea", o1, o2)
#define ADD(o1, o2)    ADD_ASM2("add", o1, o2)
#define ADDQ(o1, o2)   ADD_ASM2("addq", o1, o2)
#define SUB(o1, o2)    ADD_ASM2("sub", o1, o2)
#define SUBQ(o1, o2)   ADD_ASM2("subq", o1, o2)
#define MUL(o1)        ADD_ASM1("mul", o1)
#define DIV(o1)        ADD_ASM1("div", o1)
#define CMP(o1, o2)    ADD_ASM2("cmp", o1, o2)
#define AND(o1, o2)    ADD_ASM2("and", o1, o2)
#define OR(o1, o2)     ADD_ASM2("or", o1, o2)
#define XOR(o1, o2)    ADD_ASM2("xor", o1, o2)
#define INC(o1)        ADD_ASM1("inc", o1)
#define INCB(o1)       ADD_ASM1("incb", o1)
#define INCW(o1)       ADD_ASM1("incw", o1)
#define INCL(o1)       ADD_ASM1("incl", o1)
#define INCQ(o1)       ADD_ASM1("incq", o1)
#define DEC(o1)        ADD_ASM1("dec", o1)
#define DECB(o1)       ADD_ASM1("decb", o1)
#define DECW(o1)       ADD_ASM1("decw", o1)
#define DECL(o1)       ADD_ASM1("decl", o1)
#define DECQ(o1)       ADD_ASM1("decq", o1)
#define SHL(o1, o2)    ADD_ASM2("shl", o1, o2)
#define SHR(o1, o2)    ADD_ASM2("shr", o1, o2)
#define NEG(o1)        ADD_ASM1("neg", o1)
#define TEST(o1, o2)   ADD_ASM2("test", o1, o2)
#define PUSH(o1)       ADD_ASM1("push", o1)
#define POP(o1)        ADD_ASM1("pop", o1)
#define JMP(o1)        ADD_ASM1("jmp", o1)
#define JE(o1)         ADD_ASM1("je", o1)
#define JNE(o1)        ADD_ASM1("jne", o1)
#define JL(o1)         ADD_ASM1("jl", o1)
#define JG(o1)         ADD_ASM1("jg", o1)
#define JLE(o1)        ADD_ASM1("jle", o1)
#define JGE(o1)        ADD_ASM1("jge", o1)
#define CALL(o1)       ADD_ASM1("call", o1)
#define RET()          ADD_ASM0("ret")
#define SETE(o1)       ADD_ASM1("sete", o1)
#define SETNE(o1)      ADD_ASM1("setne", o1)
#define SETL(o1)       ADD_ASM1("setl", o1)
#define SETG(o1)       ADD_ASM1("setg", o1)
#define SETLE(o1)      ADD_ASM1("setle", o1)
#define SETGE(o1)      ADD_ASM1("setge", o1)

#define _BYTE(x)       ADD_ASM1(".byte", x)
#define _WORD(x)       ADD_ASM1(".word", x)
#define _LONG(x)       ADD_ASM1(".long", x)
#define _QUAD(x)       ADD_ASM1(".quad", x)
#define _ALIGN(x)      ADD_ASM1(".align", x)
#define _GLOBL(x)      ADD_ASM1(".globl", x)
#define _COMM(x, y)    ADD_ASM2(".comm", x, y)
#define _ASCII(x)      ADD_ASM1(".ascii", x)
#define _SECTION(x)    ADD_ASM1(".section", x)
#define _TEXT()        ADD_ASM0(".text")
#define _DATA()        ADD_ASM0(".data")
