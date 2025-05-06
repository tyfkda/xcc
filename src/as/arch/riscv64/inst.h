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
  SLL, SLLW, SLLI, SLLIW,
  SRL, SRLW, SRLI, SRLIW,
  SRA, SRAW, SRAI, SRAIW,
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

typedef struct {
  char no;  // 0~31
} Reg;

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

typedef struct Operand {
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
    int freg;
    enum RoundMode roundmode;
  };
} Operand;

typedef struct Inst {
  enum Opcode op;
  Operand opr[3];
} Inst;
