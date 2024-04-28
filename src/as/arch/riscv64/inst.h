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
  SLT, SLTU, SLTI, SLTIU,
  SEQZ, SNEZ, SLTZ, SGTZ,
  J,
  JR,
  JALR,
  BEQ, BNE, BLT, BGE, BLTU, BGEU,
  CALL,
  RET,
  ECALL,

  FADD_D, FSUB_D, FMUL_D, FDIV_D,
  FADD_S, FSUB_S, FMUL_S, FDIV_S,
  FSQRT_D, FSQRT_S,
  FSGNJ_D, FSGNJN_D, FSGNJX_D,
  FSGNJ_S, FSGNJN_S, FSGNJX_S,
  FMV_D, FNEG_D,
  FMV_S, FNEG_S,
  FMV_X_D, FMV_X_W,
  FEQ_D, FLT_D, FLE_D,
  FEQ_S, FLT_S, FLE_S,
  FLD, FLW, FSD, FSW,

  FCVT_D_W, FCVT_D_WU, FCVT_D_L, FCVT_D_LU,
  FCVT_W_D, FCVT_WU_D, FCVT_L_D, FCVT_LU_D,
  FCVT_S_W, FCVT_S_WU, FCVT_S_L, FCVT_S_LU,
  FCVT_W_S, FCVT_WU_S, FCVT_L_S, FCVT_LU_S,
  FCVT_D_S, FCVT_S_D,
};

enum RegType {
  NOREG = -1,
   X0,  X1,  X2,  X3,  X4,  X5,  X6,  X7,  X8,  X9, X10, X11, X12, X13, X14, X15,
  X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31,
};

typedef struct {
  char no;  // 0~31
} Reg;

enum FRegType {
  NOFREG = -1,
   F0,  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9, F10, F11, F12, F13, F14, F15,
  F16, F17, F18, F19, F20, F21, F22, F23, F24, F25, F26, F27, F28, F29, F30, F31,
};

enum RoundMode {
  NOROUND = -1,
  RNE,  // Round to Nearest, ties to Even
  RTZ,  // Round towards Zero
  RDN,  // Round Down (towards -Inf)
  RUP,  // Round Up (towards +Inf)
  RMM,  // Round to Nearest, ties to Max Magnitude
};

enum OperandType {
  NOOPERAND,
  REG,        // reg
  IMMEDIATE,  // 1234
  DIRECT,     // foobar + 345
  INDIRECT,   // ofs(reg)
  FREG,       // freg
  ROUNDMODE,  // rm
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
    enum FRegType freg;
    enum RoundMode roundmode;
  };
} Operand;

typedef struct Inst {
  enum Opcode op;
  Operand opr1;
  Operand opr2;
  Operand opr3;
} Inst;
