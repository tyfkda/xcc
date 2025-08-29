// aarch64 Instruction

#pragma once

#include <stdint.h>  // int64_t

typedef struct Expr Expr;

enum Opcode {
  NOOP,
  MOV, MOVK,
  ADD_R, ADD_I, SUB_R, SUB_I,
  MUL, SDIV, UDIV,
  MADD, MSUB,
  AND, ORR, EOR, EON,
  CMP_R, CMP_I, CMN_R, CMN_I,
  LSL_R, LSL_I,
  LSR_R, LSR_I,
  ASR_R, ASR_I,
  SXTB, SXTH, SXTW,
  UXTB, UXTH, UXTW,
  LDRB, LDRH, LDR, LDRSB, LDRSH, LDRSW,
  STRB, STRH, STR,
  LDP, STP,
  ADRP,
  CSET,
  B, BR,
  BEQ, BNE, BHS, BLO, BMI, BPL, BVS, BVC,
  BHI, BLS, BGE, BLT, BGT, BLE, BAL, BNV,
  CBZ, CBNZ,
  CLZ, RBIT,
  BL, BLR,
  RET,
  SVC,

  F_LDR, F_STR,
  F_LDP, F_STP,
  FMOV,
  FADD, FSUB, FMUL, FDIV,
  FCMP, FNEG,
  FSQRT,
  SCVTF, UCVTF,
  FCVT, FCVTZS, FCVTZU,
};

enum RegSize {
  REG32,
  REG64,
};

typedef struct {
  char size;  // RegSize
  char no;    // 0~31
  char sp;
} Reg;

enum CondType {
  NOCOND = -1,
  EQ, NE, HS, LO, MI, PL, VS, VC,
  HI, LS, GE, LT, GT, LE, AL, NV,
};

enum OperandType {
  NOOPERAND,
  REG,        // reg
  IMMEDIATE,  // 1234
  DIRECT,     // foobar + 345
  INDIRECT,   // indirect:   [reg,#nn]
              // pre-index:  [reg,#nn]!
              // post-index: [reg],#nn
  REGISTER_OFFSET,  // [reg,reg,#nn]
  COND,
  SHIFT,
  EXTEND,
  FREG,       // freg
};

#define LF_PAGE     (1 << 0)
#define LF_PAGEOFF  (1 << 1)
#define LF_GOT      (1 << 2)

typedef struct {
  Expr *expr;
  int flag;
} ExprWithFlag;

typedef struct Operand {
  enum OperandType type;
  union {
    Reg reg;
    int64_t immediate;
    struct {
      ExprWithFlag expr;
    } direct;
    struct {
      ExprWithFlag offset;
      Reg reg;
      int prepost;  // 0=none, 1=pre, 2=post
    } indirect;
    struct {
      Expr *scale;
      Reg base_reg;
      Reg index_reg;
      int extend;  // 0=no, 1=sxtw, 2=uxtw, 3=lsl, 4=sxtx
    } register_offset;
    enum CondType cond;
    struct {
      int option;
      int imm;
    } extend;
  };
} Operand;

typedef struct Inst {
  enum Opcode op;
  Operand opr[4];
} Inst;
