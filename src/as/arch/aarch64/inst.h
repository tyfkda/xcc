// aarch64 Instruction

#pragma once

#include <stdint.h>  // int64_t

typedef struct Expr Expr;

enum Opcode {
  NOOP,
  MOV,
  RET,
};

enum RegSize {
  REG32,
  REG64,
};

typedef struct {
  char size;  // RegSize
  char no;  // 0~31
} Reg;

enum OperandType {
  NOOPERAND,
  REG,        // reg
  IMMEDIATE,  // 1234
};

typedef struct {
  enum OperandType type;
  union {
    Reg reg;
    int64_t immediate;
  };
} Operand;

typedef struct Inst {
  enum Opcode op;
  Operand opr[4];
} Inst;
