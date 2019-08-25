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
    enum RegType reg;
    long immediate;
    const char *label;
    struct {
      enum RegType reg;
      const char *label;
      long offset;
    } indirect;
    enum RegType deref_reg;
  } u;
} Operand;

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

bool is_reg8(enum RegType reg);
bool is_reg8s(enum RegType reg);
bool is_reg8x(enum RegType reg);
bool is_reg16(enum RegType reg);
bool is_reg32(enum RegType reg);
bool is_reg32x(enum RegType reg);
bool is_reg64(enum RegType reg);
bool is_reg64x(enum RegType reg);

const char *skip_whitespace(const char *p);

enum Opcode parse_opcode(const char **pp);
enum DirectiveType parse_directive(const char **pp);
enum RegType parse_register(const char **pp);
bool parse_immediate(const char **pp, long *value);
const char *parse_label(const char **pp);
bool parse_operand(const char **pp, Operand *operand);
