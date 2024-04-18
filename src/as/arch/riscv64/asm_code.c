#include "../../../config.h"
#include "asm_code.h"

#include <assert.h>
#include <string.h>  // memcpy

#include "inst.h"
#include "parse_asm.h"
#include "util.h"

#ifndef MAKE_CODE16
#define MAKE_CODE16(inst, code, ...)  do { unsigned short buf[] = {__VA_ARGS__}; make_code16(inst, code, buf, sizeof(buf)); } while (0)
#endif

#ifndef MAKE_CODE32
#define MAKE_CODE32(inst, code, ...)  do { unsigned int buf[] = {__VA_ARGS__}; make_code32(inst, code, buf, sizeof(buf)); } while (0)
#endif

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
  return x <= ((1L << 5) - 1) && x >= -(1L << 5);
}

inline bool is_im12(int64_t x) {
  return x <= ((1L << 11) - 1) && x >= -(1L << 11);
}

inline bool is_im18(int64_t x) {
  return x <= ((1L << 17) - 1) && x >= -(1L << 17);
}

inline bool assemble_error(const ParseInfo *info, const char *message) {
  parse_error(info, message);
  return false;
}

//

#define ZERO  0
#define RA    1
#define SP    2

#define IMM(imm, t, b)  (((imm) >> (b)) & ((1 << (t - b + 1)) - 1))
#define RTYPE(funct7, rs2, rs1, funct3, rd, opcode)  MAKE_CODE32(inst, code, ((funct7) << 25) | ((rs2) << 20) | ((rs1) << 15) | ((funct3) << 12) | ((rd) << 7) | (opcode)) // R-type
#define ITYPE(imm, rs1, funct3, rd, opcode)          MAKE_CODE32(inst, code, (IMM(imm, 11, 0) << 20) | ((rs1) << 15) | ((funct3) << 12) | ((rd) << 7) | (opcode)) // I-type
#define STYPE(imm, rs2, rs1, funct3, opcode)         MAKE_CODE32(inst, code, (IMM(imm, 11, 5) << 25) | ((rs2) << 20) | ((rs1) << 15) | ((funct3) << 12) | (IMM(imm, 4, 0) << 7) | (opcode)) // S-type
#define BTYPE(imm, rs2, rs1, funct3, opcode)         MAKE_CODE32(inst, code, (IMM(imm, 12, 12) << 31) | (IMM(imm, 10, 5) << 25) | ((rs2) << 20) | ((rs1) << 15) | ((funct3) << 12) | (IMM(imm, 4, 0) << 7) | (opcode)) // B-type
#define UTYPE(imm, rd, opcode)                       MAKE_CODE32(inst, code, (IMM(imm, 31, 12) << 12) | ((rd) << 7) | (opcode)) // U-type
#define JTYPE(imm, rd, opcode)                       MAKE_CODE32(inst, code, (IMM(imm, 20, 20) << 31) | (IMM(imm, 10, 1) << 20) | (IMM(imm, 11, 11) << 19) | (IMM(imm, 19, 12) << 12) | ((rd) << 7) | (opcode)) // J-type

#define W_ADDI(rd, rs, imm)   ITYPE(imm, rs, 0x00, rd, 0x13)
#define W_ADDIW(rd, rs, imm)  ITYPE(imm, rs, 0x00, rd, 0x1b)
#define W_LB(rd, ofs, rs)     ITYPE(ofs, rs, 0x00, rd, 0x03)
#define W_LH(rd, ofs, rs)     ITYPE(ofs, rs, 0x01, rd, 0x03)
#define W_LW(rd, ofs, rs)     ITYPE(ofs, rs, 0x02, rd, 0x03)
#define W_LD(rd, ofs, rs)     ITYPE(ofs, rs, 0x03, rd, 0x03)
#define W_LBU(rd, ofs, rs)    ITYPE(ofs, rs, 0x04, rd, 0x03)
#define W_LHU(rd, ofs, rs)    ITYPE(ofs, rs, 0x05, rd, 0x03)
#define W_LWU(rd, ofs, rs)    ITYPE(ofs, rs, 0x06, rd, 0x03)
#define W_SB(rs2, ofs, rs1)   STYPE(ofs, rs2, rs1, 0x00, 0x23)
#define W_SH(rs2, ofs, rs1)   STYPE(ofs, rs2, rs1, 0x01, 0x23)
#define W_SW(rs2, ofs, rs1)   STYPE(ofs, rs2, rs1, 0x02, 0x23)
#define W_SD(rs2, ofs, rs1)   STYPE(ofs, rs2, rs1, 0x03, 0x23)
#define W_AUIPC(rd, imm)      UTYPE(imm, rd, 0x17)
#define W_JALR(rd, rs, imm)   ITYPE(imm, rs, 0x00, rd, 0x67)

#define C_LI(rd, imm)         MAKE_CODE16(inst, code, 0x4001 | (IMM(imm, 5, 5) << 12) | ((rd) << 7) | (IMM(imm, 4, 0) << 2))
#define C_LUI(rd, imm)        MAKE_CODE16(inst, code, 0x6001 | (IMM(imm, 5, 5) << 12) | ((rd) << 7) | (IMM(imm, 4, 0) << 2))
#define C_ADDI(rd, imm)       MAKE_CODE16(inst, code, 0x0001 | (IMM(imm, 5, 5) << 12) | ((rd) << 7) | (IMM(imm, 4, 0) << 2))
#define C_ADDIW(rd, imm)      MAKE_CODE16(inst, code, 0x2001 | (IMM(imm, 5, 5) << 12) | ((rd) << 7) | (IMM(imm, 4, 0) << 2))
#define C_LW(rd, imm, rs)     MAKE_CODE16(inst, code, 0x4000 | (IMM(imm, 5, 3) << 10) | (to_rvc_reg(rs) << 7) | (IMM(imm, 2, 2) << 6) | (IMM(imm, 6, 6) << 5) | (to_rvc_reg(rd) << 2))
#define C_LD(rd, imm, rs)     MAKE_CODE16(inst, code, 0x6000 | (IMM(imm, 5, 3) << 10) | (to_rvc_reg(rs) << 7) | (IMM(imm, 7, 6) << 5) | (to_rvc_reg(rd) << 2))
#define C_SW(rs2, imm, rs1)   MAKE_CODE16(inst, code, 0xc000 | (IMM(imm, 5, 3) << 10) | (to_rvc_reg(rs1) << 7) | (IMM(imm, 7, 6) << 5) | (to_rvc_reg(rs2) << 2))
#define C_SD(rs2, imm, rs1)   MAKE_CODE16(inst, code, 0xe000 | (IMM(imm, 5, 3) << 10) | (to_rvc_reg(rs1) << 7) | (IMM(imm, 7, 6) << 5) | (to_rvc_reg(rs2) << 2))
#define C_LDSP(rd, imm)       MAKE_CODE16(inst, code, 0x6002 | (IMM(imm, 5, 5) << 12) | ((rd) << 7) | (IMM(imm, 4, 3) << 5) | (IMM(imm, 8, 6) << 2))
#define C_SDSP(rs, imm)       MAKE_CODE16(inst, code, 0xe002 | (IMM(imm, 5, 3) << 10) | (IMM(imm, 8, 6) << 7) | ((rs) << 2))
#define C_JR(rs)              MAKE_CODE16(inst, code, 0x8002 | ((rs) << 7))

#define P_RET()               C_JR(RA)
#define P_LI(rd, imm)         W_ADDI(rd, ZERO, imm)

extern inline bool is_rvc_reg(int reg)  { return reg >= 8 && reg <= 15; }  // X8~X15
extern inline int to_rvc_reg(int reg)  { return reg - 8; }

static unsigned char *asm_noop(Inst *inst, Code *code) {
  UNUSED(inst);
  unsigned char *p = code->buf;
  return p;
}

static unsigned char *asm_li(Inst *inst, Code *code) {
  int rd = inst->opr1.reg.no;
  int64_t imm =inst->opr2.immediate;
  if (is_im6(imm)) {
    C_LI(rd, imm);
  } else if (is_im12(imm)) {
    P_LI(rd, imm);
  } else if (is_im18(imm)) {
    int h = imm >> 12, l = imm & 0xfff;
    C_LUI(rd, h);
    if (is_im6(imm))
      C_ADDIW(rd, l);
    else
      W_ADDIW(rd, rd, l);
  } else {
    // TODO:
    return NULL;
  }
  return code->buf;
}

static unsigned char *asm_la(Inst *inst, Code *code) {
  int rd = inst->opr1.reg.no;
  W_AUIPC(rd, 0);
  W_ADDI(rd, rd, 0);
  return code->buf;
}

static unsigned char *asm_addi(Inst *inst, Code *code) {
  int rd = inst->opr1.reg.no;
  int rs = inst->opr2.reg.no;
  int64_t imm =inst->opr3.immediate;
  if (rd == rs && is_im6(imm) && imm != 0) {
    C_ADDI(rd, imm);
  } else {
    // TODO:
    return NULL;
  }
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

static unsigned char *asm_call_d(Inst *inst, Code *code) {
  UNUSED(inst);
  W_AUIPC(RA, 0);
  W_JALR(RA, RA, 0);
  return code->buf;
}

static unsigned char *asm_ret(Inst *inst, Code *code) {
  P_RET();
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

static const AsmInstTable table_ld[] ={
    {asm_ld, REG, INDIRECT, NOOPERAND},
    {NULL} };

static const AsmInstTable table_sd[] ={
    {asm_sd, REG, INDIRECT, NOOPERAND},
    {NULL} };

static const AsmInstTable *table[] = {
  [NOOP] = (const AsmInstTable[]){ {asm_noop, NOOPERAND, NOOPERAND, NOOPERAND}, {NULL} },
  [LI] = (const AsmInstTable[]){ {asm_li, REG, IMMEDIATE, NOOPERAND}, {NULL} },
  [LA] = (const AsmInstTable[]){ {asm_la, REG, DIRECT, DIRECT}, {NULL} },
  [ADDI] = (const AsmInstTable[]){ {asm_addi, REG, REG, IMMEDIATE}, {NULL} },
  [LB] = table_ld, [LH] = table_ld, [LW] = table_ld, [LD] = table_ld,
  [LBU] = table_ld, [LHU] = table_ld, [LWU] = table_ld,
  [SB] = table_sd, [SH] = table_sd, [SW] = table_sd, [SD] = table_sd,
  [CALL] = (const AsmInstTable[]){ {asm_call_d, DIRECT, NOOPERAND, NOOPERAND}, {NULL} },
  [RET] = (const AsmInstTable[]){ {asm_ret, NOOPERAND, NOOPERAND, NOOPERAND}, {NULL} },
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
