#pragma once

#include <stdbool.h>

enum Opcode {
  NOOP,
  MOV,
  MOVSX,
  LEA,

  ADD,
  ADDQ,
  SUB,
  SUBQ,
  MUL,
  IDIV,
  NEG,
  NOT,
  INC,
  INCL,
  INCQ,
  DEC,
  DECL,
  DECQ,
  AND,
  OR,
  XOR,
  SHL,
  SHR,
  CMP,
  TEST,
  CLTD,
  CQTO,

  SETO,
  SETNO,
  SETB,
  SETAE,
  SETE,
  SETNE,
  SETBE,
  SETA,
  SETS,
  SETNS,
  SETP,
  SETNP,
  SETL,
  SETGE,
  SETLE,
  SETG,

  JMP,
  JO,
  JNO,
  JB,
  JAE,
  JE,
  JNE,
  JBE,
  JA,
  JS,
  JNS,
  JP,
  JNP,
  JL,
  JGE,
  JLE,
  JG,
  CALL,
  RET,
  PUSH,
  POP,

  INT,
  SYSCALL,
};

enum RegType {
  NOREG,

  // 8bit
  AL,
  CL,
  DL,
  BL,
  SPL,
  BPL,
  SIL,
  DIL,

  // 8bit
  R8B,
  R9B,
  R10B,
  R11B,
  R12B,
  R13B,
  R14B,
  R15B,

  // 16bit
  AX,
  CX,
  DX,
  BX,
  SP,
  BP,
  SI,
  DI,

  // 16bit
  R8W,
  R9W,
  R10W,
  R11W,
  R12W,
  R13W,
  R14W,
  R15W,

  // 32bit
  EAX,
  ECX,
  EDX,
  EBX,
  ESP,
  EBP,
  ESI,
  EDI,

  // 32bit
  R8D,
  R9D,
  R10D,
  R11D,
  R12D,
  R13D,
  R14D,
  R15D,

  // 64bit
  RAX,
  RCX,
  RDX,
  RBX,
  RSP,
  RBP,
  RSI,
  RDI,

  // 64bit
  R8,
  R9,
  R10,
  R11,
  R12,
  R13,
  R14,
  R15,

  RIP,
};

enum RegSize {
  REG8,
  REG16,
  REG32,
  REG64,
};

typedef struct {
  char size;  // RegSize
  char no;  // 0~7, or RIP
  char x;   // 0 or 1
} Reg;

enum OperandType {
  NOOPERAND,
  REG,        // %rax
  INDIRECT,   // (%rax)
  IMMEDIATE,  // $1234
  LABEL,      // foobar
  DEREF_REG,  // *%rax
};

typedef struct {
  enum OperandType type;
  union {
    Reg reg;
    long immediate;
    const char *label;
    struct {
      const char *label;
      long offset;
      Reg reg;
    } indirect;
    Reg deref_reg;
  } u;
} Operand;

typedef struct {
  enum Opcode op;
  Operand src;
  Operand dst;
} Inst;

enum DirectiveType {
  NODIRECTIVE,
  DT_ASCII,
  DT_SECTION,
  DT_TEXT,
  DT_DATA,
  DT_ALIGN,
  DT_BYTE,
  DT_WORD,
  DT_LONG,
  DT_QUAD,
  DT_COMM,
  DT_GLOBL,
  DT_EXTERN,
};

const char *skip_whitespace(const char *p);

enum Opcode parse_opcode(const char **pp);
enum DirectiveType parse_directive(const char **pp);
enum RegType parse_register(const char **pp);
bool parse_immediate(const char **pp, long *value);
const char *parse_label(const char **pp);
bool parse_operand(const char **pp, Operand *operand);
void parse_inst(const char **pp, Inst *inst);
