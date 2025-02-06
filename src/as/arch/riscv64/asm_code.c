#include "../../../config.h"
#include "asm_code.h"

#include <assert.h>
#include <string.h>  // memcpy

#include "inst.h"
#include "parse_asm.h"
#include "riscv64_code.h"
#include "util.h"

static unsigned char *asm_mv(Inst *inst, Code *code);

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

static inline bool is_im6(int64_t x) {
  return x <= ((1L << 5) - 1) && x >= -(1L << 5);
}

static inline bool is_uim6(uint64_t x) {
  return x <= ((1UL << 6) - 1);
}

static inline bool is_im12(int64_t x) {
  return x <= ((1L << 11) - 1) && x >= -(1L << 11);
}

static inline bool assemble_error(ParseInfo *info, const char *message) {
  parse_error(info, message);
  return false;
}

//

static unsigned char *asm_3r(Inst *inst, Code *code) {
  assert(inst->opr[0].type == REG);
  assert(inst->opr[1].type == REG);
  assert(inst->opr[2].type == REG);
  int rd = inst->opr[0].reg.no;
  int rs1 = inst->opr[1].reg.no;
  int rs2 = inst->opr[2].reg.no;

  if (rd == rs1) {
    if (is_rvc_reg(rd) && is_rvc_reg(rs2)) {
      switch (inst->op) {
      case ADD:   C_ADD(rd, rs2); return code->buf;
      case ADDW:  C_ADDW(rd, rs2); return code->buf;
      case SUB:   C_SUB(rd, rs2); return code->buf;
      case SUBW:  C_SUBW(rd, rs2); return code->buf;
      case AND:   C_AND(rd, rs2); return code->buf;
      case OR:    C_OR(rd, rs2); return code->buf;
      case XOR:   C_XOR(rd, rs2); return code->buf;
      default: break;
      }
    }
  } else if (rd == rs2) {
    if (is_rvc_reg(rd) && is_rvc_reg(rs1)) {
      switch (inst->op) {
      case ADD:   C_ADD(rd, rs1); return code->buf;
      case ADDW:  C_ADDW(rd, rs1); return code->buf;
      case AND:   C_AND(rd, rs1); return code->buf;
      case OR:    C_OR(rd, rs1); return code->buf;
      case XOR:   C_XOR(rd, rs1); return code->buf;
      default: break;
      }
    }
  }

  switch (inst->op) {
  case ADD:
    if (rd == rs1)
      C_ADD(rd, rs2);
    else if (rd == rs2)
      C_ADD(rd, rs1);
    else
      W_ADD(rd, rs1, rs2);
    break;
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
  case SLLW:   W_SLLW(rd, rs1, rs2); break;
  case SRL:    W_SRL(rd, rs1, rs2); break;
  case SRLW:   W_SRLW(rd, rs1, rs2); break;
  case SRA:    W_SRA(rd, rs1, rs2); break;
  case SRAW:   W_SRAW(rd, rs1, rs2); break;
  case SLT:    W_SLT(rd, rs1, rs2); break;
  case SLTU:   W_SLTU(rd, rs1, rs2); break;
  default: assert(false); return NULL;
  }
  return code->buf;
}

static unsigned char *asm_2ri(Inst *inst, Code *code) {
  assert(inst->opr[0].type == REG);
  assert(inst->opr[1].type == REG);
  assert(inst->opr[2].type == IMMEDIATE);
  int rd = inst->opr[0].reg.no;
  int rs = inst->opr[1].reg.no;
  int64_t imm =inst->opr[2].immediate;

  if (inst->op == ADDI && imm == 0) {
    if (rd != rs)
      return asm_mv(inst, code);
    return code->buf;
  }

  if (rd == rs) {
    if (inst->op == ADDI && rd == SP && is_im6(imm >> 4) && (imm & 0xf) == 0 && imm != 0) {
      C_ADDI16SP(imm);
      return code->buf;
    }

    if (is_im6(imm)) {
      switch (inst->op) {
      case ADDI:   if (imm != 0) { C_ADDI(rd, imm); return code->buf; } break;
      case ADDIW:  C_ADDIW(rd, imm); return code->buf;
      case ANDI:   if (is_rvc_reg(rd)) { C_ANDI(rd, imm); return code->buf; } break;
      default: break;
      }
    }
    if (is_uim6(imm)) {
      switch (inst->op) {
      case SLLI:   if (rd != 0 && imm != 0) { C_SLLI(rd, imm); return code->buf; } break;
      case SRLI:   if (is_rvc_reg(rd)) { C_SRLI(rd, imm); return code->buf; } break;
      case SRAI:   if (is_rvc_reg(rd)) { C_SRAI(rd, imm); return code->buf; } break;
      default: break;
      }
    }
  }
  switch (inst->op) {
  case ADDI:
  case ADDIW:
  case ANDI:
  case ORI:
  case XORI:
  case SLTI:
  case SLTIU:
    if (imm >= 2048 || imm < -2048)
      return NULL;
    switch (inst->op) {
    case ADDI:   W_ADDI(rd, rs, imm); break;
    case ADDIW:  W_ADDIW(rd, rs, imm); break;
    case ANDI:   W_ANDI(rd, rs, imm); break;
    case ORI:    W_ORI(rd, rs, imm); break;
    case XORI:   W_XORI(rd, rs, imm); break;
    case SLTI:   W_SLTI(rd, rs, imm); break;
    case SLTIU:  W_SLTIU(rd, rs, imm); break;
    default: assert(false); break;
    }
    break;

  case SLLI:
  case SLLIW:
  case SRLI:
  case SRLIW:
  case SRAI:
  case SRAIW:
    if (imm >= 64 || (inst->op == SLLIW && imm >= 32) || imm < 0)
      return NULL;
    switch (inst->op) {
    case SLLI:   W_SLLI(rd, rs, imm); break;
    case SLLIW:  W_SLLIW(rd, rs, imm); break;
    case SRLI:   W_SRLI(rd, rs, imm); break;
    case SRLIW:  W_SRLIW(rd, rs, imm); break;
    case SRAI:   W_SRAI(rd, rs, imm); break;
    case SRAIW:  W_SRAIW(rd, rs, imm); break;
    default: assert(false); break;
    }
    break;

  default: assert(false); return NULL;
  }
  return code->buf;
}

static unsigned char *asm_2r(Inst *inst, Code *code) {
  assert(inst->opr[0].type == REG);
  assert(inst->opr[1].type == REG);
  int rd = inst->opr[0].reg.no;
  int rs = inst->opr[1].reg.no;
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
  int rd = inst->opr[0].reg.no;
  int rs = inst->opr[1].reg.no;
  if (rs != ZERO)
    C_MV(rd, rs);
  else
    C_LI(rd, 0);
  return code->buf;
}

static void li_sub(Inst *inst, Code *code, int64_t imm) {
  int rd = inst->opr[0].reg.no;
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
  li_sub(inst, code, inst->opr[1].immediate);
  return code->buf;
}

static unsigned char *asm_la(Inst *inst, Code *code) {
  int rd = inst->opr[0].reg.no;
  W_AUIPC(rd, 0);
  W_ADDI(rd, rd, 0);
  return code->buf;
}

static unsigned char *asm_ld(Inst *inst, Code *code) {
  int rd = inst->opr[0].reg.no;
  Expr *offset = inst->opr[1].indirect.offset;
  if (offset == NULL || offset->kind == EX_FIXNUM) {
    int64_t ofs = offset != NULL ? offset->fixnum : 0;
    int rs = inst->opr[1].indirect.reg.no;
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
  Expr *offset = inst->opr[1].indirect.offset;
  if (offset == NULL || offset->kind == EX_FIXNUM) {
    int64_t ofs = offset != NULL ? offset->fixnum : 0;
    int rs2 = inst->opr[0].reg.no;
    int rs1 = inst->opr[1].indirect.reg.no;
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
  int rs = inst->opr[0].reg.no;
  C_JR(rs);
  return code->buf;
}

static unsigned char *asm_jalr(Inst *inst, Code *code) {
  int rs = inst->opr[0].reg.no;
  C_JALR(rs);
  return code->buf;
}

#define _BEQ   0x0
#define _BNE   0x1
#define _BLT   0x4
#define _BGE   0x5
#define _BLTU  0x6
#define _BGEU  0x7

unsigned char *asm_bxx(Inst *inst, Code *code) {
  int rs1 = inst->opr[0].reg.no;
  int rs2 = inst->opr[1].reg.no;
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
  assert(inst->opr[0].type == FREG);
  assert(inst->opr[1].type == FREG);
  assert(inst->opr[2].type == FREG);
  int rd = inst->opr[0].freg;
  int rs1 = inst->opr[1].freg;
  int rs2 = inst->opr[2].freg;

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
  assert(inst->opr[0].type == FREG);
  assert(inst->opr[1].type == FREG);
  int rd = inst->opr[0].freg;
  int rs = inst->opr[1].freg;

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
  assert(inst->opr[0].type == REG);
  assert(inst->opr[1].type == FREG);
  assert(inst->opr[2].type == FREG);
  int rd = inst->opr[0].freg;
  int rs1 = inst->opr[1].freg;
  int rs2 = inst->opr[2].freg;
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
  int rd = inst->opr[0].freg;
  Expr *offset = inst->opr[1].indirect.offset;
  if (offset == NULL || offset->kind == EX_FIXNUM) {
    int64_t ofs = offset != NULL ? offset->fixnum : 0;
    int rs = inst->opr[1].indirect.reg.no;
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
  Expr *offset = inst->opr[1].indirect.offset;
  if (offset == NULL || offset->kind == EX_FIXNUM) {
    int64_t ofs = offset != NULL ? offset->fixnum : 0;
    int rs2 = inst->opr[0].freg;
    int rs1 = inst->opr[1].indirect.reg.no;
    if (inst->op == FSD) {
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
  assert(inst->opr[0].type == FREG);
  assert(inst->opr[1].type == REG);
  int rd = inst->opr[0].freg;
  int rs = inst->opr[1].reg.no;
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
  assert(inst->opr[0].type == REG);
  assert(inst->opr[1].type == FREG);
  int rd = inst->opr[0].reg.no;
  int rs = inst->opr[1].freg;
  int rm = inst->opr[2].type == ROUNDMODE ? inst->opr[2].roundmode : 0;
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

static const AsmInstFunc table[] = {
  [NOOP] = asm_noop,
  [MV] = asm_mv,
  [LI] = asm_li,
  [LA] = asm_la,
  [ADD] = asm_3r, [ADDW] = asm_3r,
  [ADDI] = asm_2ri, [ADDIW] = asm_2ri,
  [SUB] = asm_3r, [SUBW] = asm_3r,
  [MUL] = asm_3r, [MULW] = asm_3r,
  [DIV] = asm_3r, [DIVU] = asm_3r, [DIVW] = asm_3r, [DIVUW] = asm_3r,
  [REM] = asm_3r, [REMU] = asm_3r, [REMW] = asm_3r, [REMUW] = asm_3r,
  [AND] = asm_3r, [ANDI] = asm_2ri,
  [OR] = asm_3r, [ORI] = asm_2ri,
  [XOR] = asm_3r, [XORI] = asm_2ri,
  [NEG] = asm_2r,
  [NOT] = asm_2r,
  [SEXT_B] = asm_2r, [SEXT_H] = asm_2r, [SEXT_W] = asm_2r,
  [ZEXT_B] = asm_2r, [ZEXT_H] = asm_2r, [ZEXT_W] = asm_2r,
  [SLL] = asm_3r, [SLLW] = asm_3r, [SLLI] = asm_2ri, [SLLIW] = asm_2ri,
  [SRL] = asm_3r, [SRLW] = asm_3r, [SRLI] = asm_2ri, [SRLIW] = asm_2ri,
  [SRA] = asm_3r, [SRAW] = asm_3r, [SRAI] = asm_2ri, [SRAIW] = asm_2ri,
  [LB] = asm_ld, [LH] = asm_ld, [LW] = asm_ld, [LD] = asm_ld,
  [LBU] = asm_ld, [LHU] = asm_ld, [LWU] = asm_ld,
  [SB] = asm_sd, [SH] = asm_sd, [SW] = asm_sd, [SD] = asm_sd,
  [SLT] = asm_3r, [SLTI] = asm_2ri, [SLTU] = asm_3r, [SLTIU] = asm_2ri,
  [SEQZ] = asm_2r, [SNEZ] = asm_2r, [SLTZ] = asm_2r, [SGTZ] = asm_2r,
  [J] = asm_j,
  [JR] = asm_jr,
  [JALR] = asm_jalr,
  [BEQ] = asm_bxx, [BNE] = asm_bxx, [BLT] = asm_bxx, [BGE] = asm_bxx,
  [BLTU] = asm_bxx, [BGEU] = asm_bxx,
  [CALL] = asm_call_d,
  [RET] = asm_ret,
  [ECALL] = asm_ecall,

  [FADD_D] = asm_3fr, [FSUB_D] = asm_3fr, [FMUL_D] = asm_3fr, [FDIV_D] = asm_3fr,
  [FADD_S] = asm_3fr, [FSUB_S] = asm_3fr, [FMUL_S] = asm_3fr, [FDIV_S] = asm_3fr,
  [FSQRT_D] = asm_2fr, [FSQRT_S] = asm_2fr,
  [FSGNJ_D] = asm_3fr, [FSGNJN_D] = asm_3fr, [FSGNJX_D] = asm_3fr,
  [FSGNJ_S] = asm_3fr, [FSGNJN_S] = asm_3fr, [FSGNJX_S] = asm_3fr,
  [FMV_D] = asm_2fr, [FNEG_D] = asm_2fr,
  [FMV_S] = asm_2fr, [FNEG_S] = asm_2fr,
  [FMV_X_D] = asm_if, [FMV_X_W] = asm_if,
  [FEQ_D] = asm_fcmp, [FLT_D] = asm_fcmp, [FLE_D] = asm_fcmp,
  [FEQ_S] = asm_fcmp, [FLT_S] = asm_fcmp, [FLE_S] = asm_fcmp,
  [FLD] = asm_fld, [FLW] = asm_fld, [FSD] = asm_fsd, [FSW] = asm_fsd,

  [FCVT_D_W] = asm_fi, [FCVT_D_WU] = asm_fi,
  [FCVT_D_L] = asm_fi, [FCVT_D_LU] = asm_fi,
  [FCVT_W_D] = asm_if, [FCVT_WU_D] = asm_if,
  [FCVT_L_D] = asm_if, [FCVT_LU_D] = asm_if,
  [FCVT_S_W] = asm_fi, [FCVT_S_WU] = asm_fi,
  [FCVT_S_L] = asm_fi, [FCVT_S_LU] = asm_fi,
  [FCVT_W_S] = asm_if, [FCVT_WU_S] = asm_if,
  [FCVT_L_S] = asm_if, [FCVT_LU_S] = asm_if,
  [FCVT_D_S] = asm_2fr, [FCVT_S_D] = asm_2fr,
};

void assemble_inst(Inst *inst, ParseInfo *info, Code *code) {
  code->flag = 0;
  code->len = 0;

  const AsmInstFunc *func = NULL;
  if (inst->op < (enum Opcode)ARRAY_SIZE(table) && table[inst->op] != NULL) {
    func = &table[inst->op];

    if (func != NULL) {
      unsigned char *p = (*func)(inst, code);
      if (p != NULL) {
        if (p > code->buf) {
          code->inst = inst;
          code->len = p - code->buf;
          assert((size_t)code->len <= sizeof(code->buf));
        }
        return;
      }
    }
  }

  assemble_error(info, "Illegal operand");
}
