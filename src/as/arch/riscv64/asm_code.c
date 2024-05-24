#include "../../../config.h"
#include "asm_code.h"

#include <assert.h>
#include <string.h>  // memcpy

#include "inst.h"
#include "parse_asm.h"
#include "riscv64_code.h"
#include "util.h"

void make_code16(Inst *inst, Code *code, unsigned short *buf, int len) {
  assert(code->len + len <= (int)sizeof(code->buf));
  code->inst = inst;
  memcpy(code->buf + code->len, buf, len);
  code->len += len;
}

void make_code32(Inst *inst, Code *code, unsigned int *buf, int len) {
  assert(code->len + len <= (int)sizeof(code->buf));
  code->inst = inst;
  memcpy(code->buf + code->len, buf, len);
  code->len += len;
}

inline bool is_im6(int64_t x) {
  return x <= ((1LL << 5) - 1) && x >= -(1LL << 5);
}

inline bool is_im12(int64_t x) {
  return x <= ((1LL << 11) - 1) && x >= -(1LL << 11);
}

inline bool assemble_error(const ParseInfo *info, const char *message) {
  parse_error(info, message);
  return false;
}

//

extern inline bool is_rvc_reg(int reg);
extern inline int to_rvc_reg(int reg);

static unsigned char *asm_3r(Inst *inst, Code *code) {
  assert(inst->opr1.type == REG);
  assert(inst->opr2.type == REG);
  assert(inst->opr3.type == REG);
  int rd = inst->opr1.reg.no;
  int rs1 = inst->opr2.reg.no;
  int rs2 = inst->opr3.reg.no;
  if (rd == rs1) {
    if (inst->op == ADD) {
      C_ADD(rd, rs2);
      return code->buf;
    }

    if (is_rvc_reg(rd) && is_rvc_reg(rs2)) {
      switch (inst->op) {
      case ADDW:  C_ADDW(rd, rs2); return code->buf;
      case SUB:   C_SUB(rd, rs2); return code->buf;
      case SUBW:  C_SUBW(rd, rs2); return code->buf;
      case AND:   C_AND(rd, rs2); return code->buf;
      case OR:    C_OR(rd, rs2); return code->buf;
      case XOR:   C_XOR(rd, rs2); return code->buf;
      default: break;
      }
    }
  }

  switch (inst->op) {
  case ADD:    W_ADD(rd, rs1, rs2); break;
  case ADDW:   W_ADDW(rd, rs1, rs2); break;
  case SUB:    W_SUB(rd, rs1, rs2); break;
  case SUBW:   W_SUBW(rd, rs1, rs2); break;
  case MUL:    W_MUL(rd, rs1, rs2); break;
  case MULW:   W_MULW(rd, rs1, rs2); break;
  case DIV:    W_DIV(rd, rs1, rs2); break;
  case DIVW:   W_DIVW(rd, rs1, rs2); break;
  case DIVU:   W_DIVU(rd, rs1, rs2); break;
  case DIVUW:  W_DIVUW(rd, rs1, rs2); break;
  case REM:    W_REM(rd, rs1, rs2); break;
  case REMW:   W_REMW(rd, rs1, rs2); break;
  case REMU:   W_REMU(rd, rs1, rs2); break;
  case REMUW:  W_REMUW(rd, rs1, rs2); break;
  case AND:    W_AND(rd, rs1, rs2); break;
  case OR:     W_OR(rd, rs1, rs2); break;
  case XOR:    W_XOR(rd, rs1, rs2); break;
  case SLL:    W_SLL(rd, rs1, rs2); break;
  case SRL:    W_SRL(rd, rs1, rs2); break;
  case SRA:    W_SRA(rd, rs1, rs2); break;
  case SLT:    W_SLT(rd, rs1, rs2); break;
  case SLTU:   W_SLTU(rd, rs1, rs2); break;
  default: assert(false); return NULL;
  }
  return code->buf;
}

static unsigned char *asm_2ri(Inst *inst, Code *code) {
  assert(inst->opr1.type == REG);
  assert(inst->opr2.type == REG);
  assert(inst->opr3.type == IMMEDIATE);
  int rd = inst->opr1.reg.no;
  int rs = inst->opr2.reg.no;
  int64_t imm =inst->opr3.immediate;
  if (rd == rs) {
    if (is_im6(imm)) {
      switch (inst->op) {
      case ADDI:   if (imm != 0) { C_ADDI(rd, imm); return code->buf; } break;
      case ADDIW:  C_ADDIW(rd, imm); return code->buf;
      case ANDI:   if (is_rvc_reg(rd)) { C_ANDI(rd, imm); return code->buf; } break;
      case SLLI:
        if (rd != 0 && imm != 0) {
          C_SLLI(rd, imm);
          return code->buf;
        }
        break;
      case SRLI:
        if (is_rvc_reg(rd)) {
          C_SRLI(rd, imm); return code->buf;
        }
        break;
      case SRAI:
        if (is_rvc_reg(rd)) {
          C_SRAI(rd, imm); return code->buf;
        }
        break;
      default: break;
      }
    }

    if (rd == SP && is_im6(imm >> 4) && (imm & 0xf) == 0 && imm != 0) {
      C_ADDI16SP(imm);
      return code->buf;
    }
  }
  switch (inst->op) {
  case ADDI:
  case ADDIW:
  case ANDI:
  case ORI:
  case XORI:
    if (imm >= 2048 || imm < -2048)
      return NULL;
    switch (inst->op) {
    case ADDI:   W_ADDI(rd, rs, imm); break;
    case ADDIW:  W_ADDIW(rd, rs, imm); break;
    case ANDI:   W_ANDI(rd, rs, imm); break;
    case ORI:    W_ORI(rd, rs, imm); break;
    case XORI:   W_XORI(rd, rs, imm); break;
    default: assert(false); break;
    }
    break;

  case SLLI:
  case SLLIW:
  case SRLI:
  case SRAI:
  case SLTI:
  case SLTIU:
    if (imm >= 64 || (inst->op == SLLIW && imm >= 32) || imm < 0)
      return NULL;
    switch (inst->op) {
    case SLLI:   W_SLLI(rd, rs, imm); break;
    case SLLIW:  W_SLLIW(rd, rs, imm); break;
    case SRLI:   W_SRLI(rd, rs, imm); break;
    case SRAI:   W_SRAI(rd, rs, imm); break;
    case SLTI:   W_SLTI(rd, rs, imm); break;
    case SLTIU:  W_SLTIU(rd, rs, imm); break;
    default: assert(false); break;
    }
    break;

  default: assert(false); return NULL;
  }
  return code->buf;
}

static unsigned char *asm_2r(Inst *inst, Code *code) {
  assert(inst->opr1.type == REG);
  assert(inst->opr2.type == REG);
  int rd = inst->opr1.reg.no;
  int rs = inst->opr2.reg.no;
  switch (inst->op) {
  case NEG:    P_NEG(rd, rs); break;
  case NOT:    P_NOT(rd, rs); break;
  case SEXT_B: P_SEXT_B(rd, rs); break;
  case SEXT_H: P_SEXT_H(rd, rs); break;
  case SEXT_W: P_SEXT_W(rd, rs); break;
  case ZEXT_B: P_ZEXT_B(rd, rs); break;
  case ZEXT_H: P_ZEXT_H(rd, rs); break;
  case ZEXT_W: P_ZEXT_W(rd, rs); break;
  case SEQZ:   P_SEQZ(rd, rs); break;
  case SNEZ:   P_SNEZ(rd, rs); break;
  case SLTZ:   P_SLTZ(rd, rs); break;
  case SGTZ:   P_SGTZ(rd, rs); break;
  default: assert(false); return NULL;
  }
  return code->buf;
}

static unsigned char *asm_noop(Inst *inst, Code *code) {
  UNUSED(inst);
  unsigned char *p = code->buf;
  return p;
}

static unsigned char *asm_mv(Inst *inst, Code *code) {
  int rd = inst->opr1.reg.no;
  int rs = inst->opr2.reg.no;
  if (rs != ZERO)
    C_MV(rd, rs);
  else
    C_LI(rd, 0);
  return code->buf;
}

static void li_sub(Inst *inst, Code *code, int64_t imm) {
  int rd = inst->opr1.reg.no;
  if (is_im6(imm)) {
    C_LI(rd, imm);
  } else if (is_im12(imm)) {
    P_LI(rd, imm);
  } else if (is_im32(imm)) {
    int l = imm & 0xfff;
    if (l >= 0x800) {
      l = l - 0x1000;
      imm += 1 << 12;
    }
    if (is_im6(imm >> 12))
      C_LUI(rd, imm);
    else
      W_LUI(rd, imm);
    if (is_im6(l))
      C_ADDIW(rd, l);
    else
      W_ADDIW(rd, rd, l);
  } else {
    int32_t l = (int32_t)imm & ((1 << 12) - 1);
    imm >>= 12;
    if (l >= (1 << 11)) {
      l = l - (1 << 12);
      ++imm;
    }
    li_sub(inst, code, imm);
    C_SLLI(rd, 12);
    if (is_im6(l))
      C_ADDI(rd, l);
    else
      W_ADDI(rd, rd, l);
  }
}

static unsigned char *asm_li(Inst *inst, Code *code) {
  li_sub(inst, code, inst->opr2.immediate);
  return code->buf;
}

static unsigned char *asm_la(Inst *inst, Code *code) {
  int rd = inst->opr1.reg.no;
  W_AUIPC(rd, 0);
  W_ADDI(rd, rd, 0);
  return code->buf;
}

static unsigned char *asm_ld(Inst *inst, Code *code) {
  int rd = inst->opr1.reg.no;
  Expr *offset = inst->opr2.indirect.offset;
  if (offset == NULL || offset->kind == EX_FIXNUM) {
    int64_t ofs = offset != NULL ? offset->fixnum : 0;
    int rs = inst->opr2.indirect.reg.no;
    switch (inst->op) {
    case LB:
      // TODO: Check offset range.
      W_LB(rd, ofs, rs);
      return code->buf;
    case LH:
      // TODO: Check offset range.
      W_LH(rd, ofs, rs);
      return code->buf;
    case LW:
      if (ofs >= 0 && ofs < (1 << 7) && (ofs & 3) == 0 && is_rvc_reg(rd) && is_rvc_reg(rs)) {
        C_LW(rd, ofs, rs);
        return code->buf;
      }
      // TODO: Check offset range.
      W_LW(rd, ofs, rs);
      return code->buf;
    case LD:
      if (ofs >= 0 && ofs < (1 << 9) && (ofs & 7) == 0 && rs == SP) {
        C_LDSP(rd, ofs);
        return code->buf;
      }
      if (ofs >= 0 && ofs < (1 << 8) && (ofs & 7) == 0 && is_rvc_reg(rd) && is_rvc_reg(rs)) {
        C_LD(rd, ofs, rs);
        return code->buf;
      }
      // TODO: Check offset range.
      W_LD(rd, ofs, rs);
      return code->buf;
    case LBU:
      // TODO: Check offset range.
      W_LBU(rd, ofs, rs);
      return code->buf;
    case LHU:
      // TODO: Check offset range.
      W_LHU(rd, ofs, rs);
      return code->buf;
    case LWU:
      // TODO: Check offset range.
      W_LWU(rd, ofs, rs);
      return code->buf;
    default: break;
    }
  }
  return NULL;
}

static unsigned char *asm_sd(Inst *inst, Code *code) {
  Expr *offset = inst->opr2.indirect.offset;
  if (offset == NULL || offset->kind == EX_FIXNUM) {
    int64_t ofs = offset != NULL ? offset->fixnum : 0;
    int rs2 = inst->opr1.reg.no;
    int rs1 = inst->opr2.indirect.reg.no;
    switch (inst->op) {
    case SB:
      // TODO: Check offset range.
      W_SB(rs2, ofs, rs1);
      return code->buf;
    case SH:
      // TODO: Check offset range.
      W_SH(rs2, ofs, rs1);
      return code->buf;
    case SW:
      if (ofs >= 0 && ofs < (1 << 7) && (ofs & 3) == 0 && is_rvc_reg(rs1) && is_rvc_reg(rs2)) {
        C_SW(rs2, ofs, rs1);
        return code->buf;
      }
      // TODO: Check offset range.
      W_SW(rs2, ofs, rs1);
      return code->buf;
    case SD:
      if (ofs >= 0 && ofs < (1 << 9) && (ofs & 7) == 0 && rs1 == SP) {
        C_SDSP(rs2, ofs);
        return code->buf;
      }
      if (ofs >= 0 && ofs < (1 << 8) && (ofs & 7) == 0 && is_rvc_reg(rs1) && is_rvc_reg(rs2)) {
        C_SD(rs2, ofs, rs1);
        return code->buf;
      }
      // TODO: Check offset range.
      W_SD(rs2, ofs, rs1);
      return code->buf;
    default: break;
    }
  }
  return NULL;
}

static unsigned char *asm_j(Inst *inst, Code *code) {
  // imm[11|4|9:8|10|6|7|3:1|5]
  C_J();
  return code->buf;
}

static unsigned char *asm_jr(Inst *inst, Code *code) {
  int rs = inst->opr1.reg.no;
  C_JR(rs);
  return code->buf;
}

static unsigned char *asm_jalr(Inst *inst, Code *code) {
  int rs = inst->opr1.reg.no;
  C_JALR(rs);
  return code->buf;
}

#define _BEQ   0x0
#define _BNE   0x1
#define _BLT   0x4
#define _BGE   0x5
#define _BLTU  0x6
#define _BGEU  0x7

static unsigned char *asm_bxx(Inst *inst, Code *code) {
  int rs1 = inst->opr1.reg.no;
  int rs2 = inst->opr2.reg.no;
  if (rs2 == ZERO && is_rvc_reg(rs1)) {
    switch (inst->op) {
    case BEQ:  C_BEQZ(rs1); return code->buf;
    case BNE:  C_BNEZ(rs1); return code->buf;
    default: break;
    }
  }
  code->flag |= INST_LONG_OFFSET;
  static const int kFunct3Table[] = { _BEQ, _BNE, _BLT, _BGE, _BLTU, _BGEU };
  int funct3 = kFunct3Table[inst->op - BEQ];
  W_BXX(funct3, rs1, rs2, 0);
  return code->buf;
}

static unsigned char *asm_call_d(Inst *inst, Code *code) {
  W_AUIPC(RA, 0);
  W_JALR(RA, RA, 0);
  return code->buf;
}

static unsigned char *asm_ret(Inst *inst, Code *code) {
  P_RET();
  return code->buf;
}

static unsigned char *asm_ecall(Inst *inst, Code *code) {
  W_ECALL();
  return code->buf;
}

static unsigned char *asm_3fr(Inst *inst, Code *code) {
  assert(inst->opr1.type == FREG);
  assert(inst->opr2.type == FREG);
  assert(inst->opr3.type == FREG);
  int rd = inst->opr1.freg;
  int rs1 = inst->opr2.freg;
  int rs2 = inst->opr3.freg;

  switch (inst->op) {
  case FADD_D:    W_FADD_D(rd, rs1, rs2); break;
  case FSUB_D:    W_FSUB_D(rd, rs1, rs2); break;
  case FMUL_D:    W_FMUL_D(rd, rs1, rs2); break;
  case FDIV_D:    W_FDIV_D(rd, rs1, rs2); break;
  case FADD_S:    W_FADD_S(rd, rs1, rs2); break;
  case FSUB_S:    W_FSUB_S(rd, rs1, rs2); break;
  case FMUL_S:    W_FMUL_S(rd, rs1, rs2); break;
  case FDIV_S:    W_FDIV_S(rd, rs1, rs2); break;
  case FSGNJ_D:   W_FSGNJ_D(rd, rs1, rs2); break;
  case FSGNJN_D:  W_FSGNJN_D(rd, rs1, rs2); break;
  case FSGNJX_D:  W_FSGNJX_D(rd, rs1, rs2); break;
  case FSGNJ_S:   W_FSGNJ_S(rd, rs1, rs2); break;
  case FSGNJN_S:  W_FSGNJN_S(rd, rs1, rs2); break;
  case FSGNJX_S:  W_FSGNJX_S(rd, rs1, rs2); break;
  default: assert(false); return NULL;
  }
  return code->buf;
}

static unsigned char *asm_2fr(Inst *inst, Code *code) {
  assert(inst->opr1.type == FREG);
  assert(inst->opr2.type == FREG);
  int rd = inst->opr1.freg;
  int rs = inst->opr2.freg;

  switch (inst->op) {
  case FSQRT_D:   W_FSQRT_D(rd, rs); break;
  case FSQRT_S:   W_FSQRT_S(rd, rs); break;
  case FMV_D:     P_FMV_D(rd, rs); break;
  case FNEG_D:    P_FNEG_D(rd, rs); break;
  case FMV_S:     P_FMV_S(rd, rs); break;
  case FNEG_S:    P_FNEG_S(rd, rs); break;
  case FCVT_D_S:  W_FCVT_D_S(rd, rs); break;
  case FCVT_S_D:  W_FCVT_S_D(rd, rs); break;
  default: assert(false); return NULL;
  }
  return code->buf;
}

static unsigned char *asm_fcmp(Inst *inst, Code *code) {
  assert(inst->opr1.type == REG);
  assert(inst->opr2.type == FREG);
  assert(inst->opr3.type == FREG);
  int rd = inst->opr1.freg;
  int rs1 = inst->opr2.freg;
  int rs2 = inst->opr3.freg;
  switch (inst->op) {
  case FEQ_D:  W_FEQ_D(rd, rs1, rs2); break;
  case FLT_D:  W_FLT_D(rd, rs1, rs2); break;
  case FLE_D:  W_FLE_D(rd, rs1, rs2); break;
  case FEQ_S:  W_FEQ_S(rd, rs1, rs2); break;
  case FLT_S:  W_FLT_S(rd, rs1, rs2); break;
  case FLE_S:  W_FLE_S(rd, rs1, rs2); break;
  default: assert(false); return NULL;
  }
  return code->buf;
}

static unsigned char *asm_fld(Inst *inst, Code *code) {
  int rd = inst->opr1.freg;
  Expr *offset = inst->opr2.indirect.offset;
  if (offset == NULL || offset->kind == EX_FIXNUM) {
    int64_t ofs = offset != NULL ? offset->fixnum : 0;
    int rs = inst->opr2.indirect.reg.no;
    if (inst->op == FLD) {
      if (ofs >= 0 && ofs < (1 << 9) && (ofs & 7) == 0 && rs == SP) {
        C_FLDSP(rd, ofs);
        return code->buf;
      }
      if (ofs >= 0 && ofs < (1 << 8) && (ofs & 7) == 0 && is_rvc_freg(rd) && is_rvc_freg(rs)) {
        C_FLD(rd, ofs, rs);
        return code->buf;
      }
    }
    // TODO: Check offset range.
    switch (inst->op) {
    case FLD:  W_FLD(rd, ofs, rs); break;
    case FLW:  W_FLW(rd, ofs, rs); break;
    default: assert(false); break;
    }
    return code->buf;
  }
  return NULL;
}

static unsigned char *asm_fsd(Inst *inst, Code *code) {
  Expr *offset = inst->opr2.indirect.offset;
  if (offset == NULL || offset->kind == EX_FIXNUM) {
    int64_t ofs = offset != NULL ? offset->fixnum : 0;
    int rs2 = inst->opr1.freg;
    int rs1 = inst->opr2.indirect.reg.no;
    if (inst->op == FLD) {
      if (ofs >= 0 && ofs < (1 << 9) && (ofs & 7) == 0 && rs1 == SP) {
        C_FSDSP(rs2, ofs);
        return code->buf;
      }
      if (ofs >= 0 && ofs < (1 << 8) && (ofs & 7) == 0 && is_rvc_freg(rs1) && is_rvc_freg(rs2)) {
        C_FSD(rs2, ofs, rs1);
        return code->buf;
      }
    }
    // TODO: Check offset range.
    switch (inst->op) {
    case FSD:  W_FSD(rs2, ofs, rs1); break;
    case FSW:  W_FSW(rs2, ofs, rs1); break;
    default: assert(false); break;
    }
    return code->buf;
  }
  return NULL;
}

static unsigned char *asm_fi(Inst *inst, Code *code) {
  assert(inst->opr1.type == FREG);
  assert(inst->opr2.type == REG);
  int rd = inst->opr1.freg;
  int rs = inst->opr2.reg.no;
  switch (inst->op) {
  case FCVT_D_W:  W_FCVT_D_W(rd, rs); break;
  case FCVT_D_WU: W_FCVT_D_WU(rd, rs); break;
  case FCVT_D_L:  W_FCVT_D_L(rd, rs); break;
  case FCVT_D_LU: W_FCVT_D_LU(rd, rs); break;
  case FCVT_S_W:  W_FCVT_S_W(rd, rs); break;
  case FCVT_S_WU: W_FCVT_S_WU(rd, rs); break;
  case FCVT_S_L:  W_FCVT_S_L(rd, rs); break;
  case FCVT_S_LU: W_FCVT_S_LU(rd, rs); break;
  default: assert(false); return NULL;
  }
  return code->buf;
}

static unsigned char *asm_if(Inst *inst, Code *code) {
  assert(inst->opr1.type == REG);
  assert(inst->opr2.type == FREG);
  int rd = inst->opr1.reg.no;
  int rs = inst->opr2.freg;
  int rm = inst->opr3.type == ROUNDMODE ? inst->opr3.roundmode : 0;
  switch (inst->op) {
  case FMV_X_D:   W_FMV_X_D(rd, rs); break;
  case FMV_X_W:   W_FMV_X_W(rd, rs); break;
  case FCVT_W_D:  W_FCVT_W_D(rd, rs, rm); break;
  case FCVT_WU_D: W_FCVT_WU_D(rd, rs, rm); break;
  case FCVT_L_D:  W_FCVT_L_D(rd, rs, rm); break;
  case FCVT_LU_D: W_FCVT_LU_D(rd, rs, rm); break;
  case FCVT_W_S:  W_FCVT_W_S(rd, rs, rm); break;
  case FCVT_WU_S: W_FCVT_WU_S(rd, rs, rm); break;
  case FCVT_L_S:  W_FCVT_L_S(rd, rs, rm); break;
  case FCVT_LU_S: W_FCVT_LU_S(rd, rs, rm); break;
  default: assert(false); return NULL;
  }
  return code->buf;
}

////////////////////////////////////////////////

typedef unsigned char *(*AsmInstFunc)(Inst *inst, Code *code);
typedef struct {
  AsmInstFunc func;
  enum OperandType opr1_type;
  enum OperandType opr2_type;
  enum OperandType opr3_type;
  int flag;
} AsmInstTable;

static const AsmInstTable table_3r[] ={
    {asm_3r, REG, REG, REG},
    {NULL} };

static const AsmInstTable table_2ri[] ={
    {asm_2ri, REG, REG, IMMEDIATE},
    {NULL} };

static const AsmInstTable table_2r[] ={
    {asm_2r, REG, REG, NOOPERAND},
    {NULL} };

static const AsmInstTable table_ld[] ={
    {asm_ld, REG, INDIRECT, NOOPERAND},
    {NULL} };

static const AsmInstTable table_sd[] ={
    {asm_sd, REG, INDIRECT, NOOPERAND},
    {NULL} };

static const AsmInstTable table_bxx[] ={
    {asm_bxx, REG, REG, DIRECT},
    {NULL} };

static const AsmInstTable table_3fr[] ={
    {asm_3fr, FREG, FREG, FREG},
    {NULL} };

static const AsmInstTable table_2fr[] ={
    {asm_2fr, FREG, FREG, NOOPERAND},
    {NULL} };

static const AsmInstTable table_fcmp[] ={
    {asm_fcmp, REG, FREG, FREG},
    {NULL} };

static const AsmInstTable table_fld[] ={
    {asm_fld, FREG, INDIRECT, NOOPERAND},
    {NULL} };

static const AsmInstTable table_fsd[] ={
    {asm_fsd, FREG, INDIRECT, NOOPERAND},
    {NULL} };

static const AsmInstTable table_fi[] ={
    {asm_fi, FREG, REG, NOOPERAND},
    {NULL} };

static const AsmInstTable table_if[] ={
    {asm_if, REG, FREG, NOOPERAND},
    {asm_if, REG, FREG, ROUNDMODE},
    {NULL} };

static const AsmInstTable *table[] = {
  [NOOP] = (const AsmInstTable[]){ {asm_noop, NOOPERAND, NOOPERAND, NOOPERAND}, {NULL} },
  [MV] = (const AsmInstTable[]){ {asm_mv, REG, REG, NOOPERAND}, {NULL} },
  [LI] = (const AsmInstTable[]){ {asm_li, REG, IMMEDIATE, NOOPERAND}, {NULL} },
  [LA] = (const AsmInstTable[]){ {asm_la, REG, DIRECT, DIRECT}, {NULL} },
  [ADD] = table_3r, [ADDW] = table_3r,
  [ADDI] = table_2ri, [ADDIW] = table_2ri,
  [SUB] = table_3r, [SUBW] = table_3r,
  [MUL] = table_3r, [MULW] = table_3r,
  [DIV] = table_3r, [DIVU] = table_3r, [DIVW] = table_3r, [DIVUW] = table_3r,
  [REM] = table_3r, [REMU] = table_3r, [REMW] = table_3r, [REMUW] = table_3r,
  [AND] = table_3r, [ANDI] = table_2ri,
  [OR] = table_3r, [ORI] = table_2ri,
  [XOR] = table_3r, [XORI] = table_2ri,
  [NEG] = table_2r,
  [NOT] = table_2r,
  [SEXT_B] = table_2r, [SEXT_H] = table_2r, [SEXT_W] = table_2r,
  [ZEXT_B] = table_2r, [ZEXT_H] = table_2r, [ZEXT_W] = table_2r,
  [SLL] = table_3r, [SLLI] = table_2ri, [SLLIW] = table_2ri,
  [SRL] = table_3r, [SRLI] = table_2ri, [SRLIW] = table_2ri,
  [SRA] = table_3r, [SRAI] = table_2ri,
  [LB] = table_ld, [LH] = table_ld, [LW] = table_ld, [LD] = table_ld,
  [LBU] = table_ld, [LHU] = table_ld, [LWU] = table_ld,
  [SB] = table_sd, [SH] = table_sd, [SW] = table_sd, [SD] = table_sd,
  [SLT] = table_3r, [SLTI] = table_2ri, [SLTU] = table_3r, [SLTIU] = table_2ri,
  [SEQZ] = table_2r, [SNEZ] = table_2r, [SLTZ] = table_2r, [SGTZ] = table_2r,
  [J] = (const AsmInstTable[]){ {asm_j, DIRECT, NOOPERAND, NOOPERAND}, {NULL} },
  [JR] = (const AsmInstTable[]){ {asm_jr, REG, NOOPERAND, NOOPERAND}, {NULL} },
  [JALR] = (const AsmInstTable[]){ {asm_jalr, REG, NOOPERAND, NOOPERAND}, {NULL} },
  [BEQ] = table_bxx, [BNE] = table_bxx, [BLT] = table_bxx, [BGE] = table_bxx,
  [BLTU] = table_bxx, [BGEU] = table_bxx,
  [CALL] = (const AsmInstTable[]){ {asm_call_d, DIRECT, NOOPERAND, NOOPERAND}, {NULL} },
  [RET] = (const AsmInstTable[]){ {asm_ret, NOOPERAND, NOOPERAND, NOOPERAND}, {NULL} },
  [ECALL] = (const AsmInstTable[]){ {asm_ecall, NOOPERAND, NOOPERAND, NOOPERAND}, {NULL} },

  [FADD_D] = table_3fr, [FSUB_D] = table_3fr, [FMUL_D] = table_3fr, [FDIV_D] = table_3fr,
  [FADD_S] = table_3fr, [FSUB_S] = table_3fr, [FMUL_S] = table_3fr, [FDIV_S] = table_3fr,
  [FSQRT_D] = table_2fr, [FSQRT_S] = table_2fr,
  [FSGNJ_D] = table_3fr, [FSGNJN_D] = table_3fr, [FSGNJX_D] = table_3fr,
  [FSGNJ_S] = table_3fr, [FSGNJN_S] = table_3fr, [FSGNJX_S] = table_3fr,
  [FMV_D] = table_2fr, [FNEG_D] = table_2fr,
  [FMV_S] = table_2fr, [FNEG_S] = table_2fr,
  [FMV_X_D] = table_if, [FMV_X_W] = table_if,
  [FEQ_D] = table_fcmp, [FLT_D] = table_fcmp, [FLE_D] = table_fcmp,
  [FEQ_S] = table_fcmp, [FLT_S] = table_fcmp, [FLE_S] = table_fcmp,
  [FLD] = table_fld, [FLW] = table_fld, [FSD] = table_fsd, [FSW] = table_fsd,

  [FCVT_D_W] = table_fi, [FCVT_D_WU] = table_fi,
  [FCVT_D_L] = table_fi, [FCVT_D_LU] = table_fi,
  [FCVT_W_D] = table_if, [FCVT_WU_D] = table_if,
  [FCVT_L_D] = table_if, [FCVT_LU_D] = table_if,
  [FCVT_S_W] = table_fi, [FCVT_S_WU] = table_fi,
  [FCVT_S_L] = table_fi, [FCVT_S_LU] = table_fi,
  [FCVT_W_S] = table_if, [FCVT_WU_S] = table_if,
  [FCVT_L_S] = table_if, [FCVT_LU_S] = table_if,
  [FCVT_D_S] = table_2fr, [FCVT_S_D] = table_2fr,
};

void assemble_inst(Inst *inst, const ParseInfo *info, Code *code) {
  code->flag = 0;
  code->len = 0;

  const AsmInstTable *pt = NULL;
  if (inst->op < (enum Opcode)ARRAY_SIZE(table) && table[inst->op] != NULL) {
    for (const AsmInstTable *p = table[inst->op]; p->func != NULL; ++p) {
      if (inst->opr1.type == p->opr1_type && inst->opr2.type == p->opr2_type && inst->opr3.type == p->opr3_type) {
        pt = p;
        break;
      }
    }
  }

  if (pt != NULL) {
    unsigned char *p = (*pt->func)(inst, code);
    if (p != NULL) {
      if (p > code->buf) {
        code->inst = inst;
        code->len = p - code->buf;
        assert((size_t)code->len <= sizeof(code->buf));
      }
      return;
    }
  }

  assemble_error(info, "Illegal opeand");
}
