// riscv64 Instruction

#pragma once

#include <stdint.h>  // int64_t

typedef struct Expr Expr;

// Must match the order with kOpTable in parse_riscv64.c
enum Opcode {
  NOOP,
  MV,
  LI,
  LA,
  ADD, ADDW,
  ADDI, ADDIW,
  SUB, SUBW,
  MUL, MULW,
  DIV, DIVU, DIVW, DIVUW,
  REM, REMU, REMW, REMUW,
  AND, ANDI,
  OR, ORI,
  XOR, XORI,
  NEG,
  NOT,
  SEXT_B, SEXT_H, SEXT_W,
  ZEXT_B, ZEXT_H, ZEXT_W,
  SLL, SLLI, SLLIW,
  SRL, SRLI, SRLIW,
  SRA, SRAI,
  LB, LH, LW, LD,
  LBU, LHU, LWU,
  SB, SH, SW, SD,
  J,
  JR,
  JALR,
  BEQ, BNE, BLT, BGE, BLTU, BGEU,
  CALL,
  RET,
};

enum RegType {
  NOREG,

   X0,  X1,  X2,  X3,  X4,  X5,  X6,  X7,  X8,  X9, X10, X11, X12, X13, X14, X15,
  X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31,
};

typedef struct {
  char no;  // 0~31
} Reg;

enum OperandType {
  NOOPERAND,
  REG,        // reg
  IMMEDIATE,  // 1234
  DIRECT,     // foobar + 345
  INDIRECT,   // ofs(reg)
};

typedef struct {
  enum OperandType type;
  union {
    Reg reg;
    int64_t immediate;
    struct {
      Expr *expr;
    } direct;
    struct {
      Expr *offset;
      Reg reg;
    } indirect;
  };
} Operand;

typedef struct Inst {
  enum Opcode op;
  Operand opr1;
  Operand opr2;
  Operand opr3;
} Inst;

inline bool is_rvc_reg(int reg)  { return reg >= 8 && reg <= 15; }  // X8~X15
inline int to_rvc_reg(int reg)  { return reg - 8; }
