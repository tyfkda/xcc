#include "../../../config.h"
#include "parse_asm.h"

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <strings.h>

#include "table.h"
#include "util.h"

enum RawOpcode {
  R_NOOP,
  R_MV,
  R_LI,
  R_LA,
  R_ADD, R_ADDW,
  R_ADDI, R_ADDIW,
  R_SUB, R_SUBW,
  R_MUL, R_MULW,
  R_DIV, R_DIVU, R_DIVW, R_DIVUW,
  R_REM, R_REMU, R_REMW, R_REMUW,
  R_AND, R_ANDI,
  R_OR, R_ORI,
  R_XOR, R_XORI,
  R_NEG,
  R_NOT,
  R_SEXT_B, R_SEXT_H, R_SEXT_W,
  R_ZEXT_B, R_ZEXT_H, R_ZEXT_W,
  R_SLL, R_SLLI, R_SLLIW,
  R_SRL, R_SRLI, R_SRLIW,
  R_SRA, R_SRAI,
  R_LB, R_LH, R_LW, R_LD,
  R_LBU, R_LHU, R_LWU,
  R_SB, R_SH, R_SW, R_SD,
  R_SLT, R_SLTU, R_SLTI, R_SLTIU,
  R_SEQZ, R_SNEZ, R_SLTZ, R_SGTZ,
  R_J,
  R_JR,
  R_JALR,
  R_BEQ, R_BNE, R_BLT, R_BGE, R_BLTU, R_BGEU,
  R_CALL,
  R_RET,
  R_ECALL,

  R_FADD_D, R_FSUB_D, R_FMUL_D, R_FDIV_D,
  R_FADD_S, R_FSUB_S, R_FMUL_S, R_FDIV_S,
  R_FSQRT_D, R_FSQRT_S,
  R_FSGNJ_D, R_FSGNJN_D, R_FSGNJX_D,
  R_FSGNJ_S, R_FSGNJN_S, R_FSGNJX_S,
  R_FMV_D, R_FNEG_D,
  R_FMV_S, R_FNEG_S,
  R_FMV_X_D, R_FMV_X_W,
  R_FEQ_D, R_FLT_D, R_FLE_D,
  R_FEQ_S, R_FLT_S, R_FLE_S,
  R_FLD, R_FLW, R_FSD, R_FSW,

  R_FCVT_D_W, R_FCVT_D_WU, R_FCVT_D_L, R_FCVT_D_LU,
  R_FCVT_W_D, R_FCVT_WU_D, R_FCVT_L_D, R_FCVT_LU_D,
  R_FCVT_S_W, R_FCVT_S_WU, R_FCVT_S_L, R_FCVT_S_LU,
  R_FCVT_W_S, R_FCVT_WU_S, R_FCVT_L_S, R_FCVT_LU_S,
  R_FCVT_D_S, R_FCVT_S_D,
};

const char *kRawOpTable[] = {
  "mv",
  "li",
  "la",
  "add", "addw",
  "addi", "addiw",
  "sub", "subw",
  "mul", "mulw",
  "div", "divu", "divw", "divuw",
  "rem", "remu", "remw", "remuw",
  "and", "andi",
  "or", "ori",
  "xor", "xori",
  "neg",
  "not",
  "sext.b", "sext.h", "sext.w",
  "zext.b", "zext.h", "zext.w",
  "sll", "slli", "slliw",
  "srl", "srli", "srliw",
  "sra", "srai",
  "lb", "lh", "lw", "ld",
  "lbu", "lhu", "lwu",
  "sb", "sh", "sw", "sd",
  "slt", "sltu", "slti", "sltiu",
  "seqz", "snez", "sltz", "sgtz",
  "j",
  "jr",
  "jalr",
  "beq", "bne", "blt", "bge", "bltu", "bgeu",
  "call",
  "ret",
  "ecall",

  "fadd.d", "fsub.d", "fmul.d", "fdiv.d",
  "fadd.s", "fsub.s", "fmul.s", "fdiv.s",
  "fsqrt.d", "fsqrt.s",
  "fsgnj.d", "fsgnjn.d", "fsgnjx.d",
  "fsgnj.s", "fsgnjn.s", "fsgnjx.s",
  "fmv.d", "fneg.d",
  "fmv.s", "fneg.s",
  "fmv.x.d", "fmv.x.w",
  "feq.d", "flt.d", "fle.d",
  "feq.s", "flt.s", "fle.s",
  "fld", "flw", "fsd", "fsw",
  "fcvt.d.w", "fcvt.d.wu", "fcvt.d.l", "fcvt.d.lu",
  "fcvt.w.d", "fcvt.wu.d", "fcvt.l.d", "fcvt.lu.d",
  "fcvt.s.w", "fcvt.s.wu", "fcvt.s.l", "fcvt.s.lu",
  "fcvt.w.s", "fcvt.wu.s", "fcvt.l.s", "fcvt.lu.s",
  "fcvt.d.s", "fcvt.s.d",
  NULL,
};

enum RegType {
  NOREG = -1,
   X0,  X1,  X2,  X3,  X4,  X5,  X6,  X7,  X8,  X9, X10, X11, X12, X13, X14, X15,
  X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31,
};

enum FRegType {
  NOFREG = -1,
   F0,  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9, F10, F11, F12, F13, F14, F15,
  F16, F17, F18, F19, F20, F21, F22, F23, F24, F25, F26, F27, F28, F29, F30, F31,
};

#define ZEROREG  X0
#define RA       X1
#define SP       X2
#define GP       X3
#define TP       X4
#define T0       X5
#define T1       X6
#define T2       X7
#define FP       X8
#define S1       X9
#define A0      X10
#define A1      X11
#define A2      X12
#define A3      X13
#define A4      X14
#define A5      X15
#define A6      X16
#define A7      X17
#define S2      X18
#define S3      X19
#define S4      X20
#define S5      X21
#define S6      X22
#define S7      X23
#define S8      X24
#define S9      X25
#define S10     X26
#define S11     X27
#define T3      X28
#define T4      X29
#define T5      X30
#define T6      X31

static const struct {
  const char *name;
  enum RegType reg;
} kRegisters[] = {
  {"x0", X0},    {"x1", X1},    {"x2", X2},    {"x3", X3},
  {"x4", X4},    {"x5", X5},    {"x6", X6},    {"x7", X7},
  {"x8", X8},    {"x9", X9},    {"x10", X10},  {"x11", X11},
  {"x12", X12},  {"x13", X13},  {"x14", X14},  {"x15", X15},
  {"x16", X16},  {"x17", X17},  {"x18", X18},  {"x19", X19},
  {"x20", X20},  {"x21", X21},  {"x22", X22},  {"x23", X23},
  {"x24", X24},  {"x25", X25},  {"x26", X26},  {"x27", X27},
  {"x28", X28},  {"x29", X29},  {"x30", X30},  {"x31", X31},

  // Alias
  {"zero", ZEROREG},  {"ra", RA},  {"sp", SP},  {"gp", GP},
  {"tp", TP},  {"t0", T0},  {"t1", T1},  {"t2", T2},
  {"fp", FP},  {"s1", S1},  {"a0", A0},  {"a1", A1},
  {"a2", A2},  {"a3", A3},  {"a4", A4},  {"a5", A5},
  {"a6", A6},  {"a7", A7},  {"s2", S2},  {"s3", S3},
  {"s4", S4},  {"s5", S5},  {"s6", S6},  {"s7", S7},
  {"s8", S8},  {"s9", S9},  {"s10", S10},  {"s11", S11},
  {"t3", T3},  {"t4", T4},  {"t5", T5},  {"t6", T6},
};

#define FT0   F0
#define FT1   F1
#define FT2   F2
#define FT3   F3
#define FT4   F4
#define FT5   F5
#define FT6   F6
#define FT7   F7
#define FS0   F8
#define FS1   F9
#define FA0   F10
#define FA1   F11
#define FA2   F12
#define FA3   F13
#define FA4   F14
#define FA5   F15
#define FA6   F16
#define FA7   F17
#define FS2   F18
#define FS3   F19
#define FS4   F20
#define FS5   F21
#define FS6   F22
#define FS7   F23
#define FS8   F24
#define FS9   F25
#define FS10  F26
#define FS11  F27
#define FT8   F28
#define FT9   F29
#define FT10  F30
#define FT11  F31

static const struct {
  const char *name;
  enum FRegType reg;
} kFRegisters[] = {
  {"f0", F0},    {"f1", F1},    {"f2", F2},    {"f3", F3},
  {"f4", F4},    {"f5", F5},    {"f6", F6},    {"f7", F7},
  {"f8", F8},    {"f9", F9},    {"f10", F10},  {"f11", F11},
  {"f12", F12},  {"f13", F13},  {"f14", F14},  {"f15", F15},
  {"f16", F16},  {"f17", F17},  {"f18", F18},  {"f19", F19},
  {"f20", F20},  {"f21", F21},  {"f22", F22},  {"f23", F23},
  {"f24", F24},  {"f25", F25},  {"f26", F26},  {"f27", F27},
  {"f28", F28},  {"f29", F29},  {"f30", F30},  {"f31", F31},

  // Alias
  {"ft0", FT0},  {"ft1", FT1},  {"ft2", FT2},  {"ft3", FT3},
  {"ft4", FT4},  {"ft5", FT5},  {"ft6", FT6},  {"ft7", FT7},
  {"fs0", FS0},  {"fs1", FS1},  {"fa0", FA0},  {"fa1", FA1},
  {"fa2", FA2},  {"fa3", FA3},  {"fa4", FA4},  {"fa5", FA5},
  {"fa6", FA6},  {"fa7", FA7},  {"fs2", FS2},  {"fs3", FS3},
  {"fs4", FS4},  {"fs5", FS5},  {"fs6", FS6},  {"fs7", FS7},
  {"fs8", FS8},  {"fs9", FS9},  {"fs10", FS10},  {"fs11", FS11},
  {"ft8", FT8},  {"ft9", FT9},  {"ft10", FT10},  {"ft11", FT11},
};

static const struct {
  const char *name;
  enum RoundMode mode;
} kRoundModes[] = {
  {"rne", RNE},
  {"rtz", RTZ},
  {"rdn", RDN},
  {"rup", RUP},
  {"rmm", RMM},
};

#define R64  (1 << 0)
#define F64  (1 << 1)
#define IMM  (1 << 2)
#define IND  (1 << 3)
#define EXP  (1 << 4)
#define RND  (1 << 10)

static enum RegType find_register(const char **pp) {
  const char *p = *pp;
  for (int i = 0; i < (int)ARRAY_SIZE(kRegisters); ++i) {
    const char *name = kRegisters[i].name;
    size_t n = strlen(name);
    if (strncasecmp(p, name, n) == 0 && !is_label_chr(p[n])) {
      *pp = p + n;
      return kRegisters[i].reg;
    }
  }
  return NOREG;
}

static enum FRegType find_fregister(const char **pp) {
  const char *p = *pp;
  for (int i = 0; i < (int)ARRAY_SIZE(kFRegisters); ++i) {
    const char *name = kFRegisters[i].name;
    size_t n = strlen(name);
    if (strncasecmp(p, name, n) == 0 && !is_label_chr(p[n])) {
      *pp = p + n;
      return kFRegisters[i].reg;
    }
  }
  return NOFREG;
}

static enum RoundMode find_round_mode(const char **pp) {
  const char *p = *pp;
  for (int i = 0; i < (int)ARRAY_SIZE(kRoundModes); ++i) {
    const char *name = kRoundModes[i].name;
    size_t n = strlen(name);
    if (strncasecmp(p, name, n) == 0 && !is_label_chr(p[n])) {
      *pp = p + n;
      return kRoundModes[i].mode;
    }
  }
  return NOROUND;
}

static bool parse_indirect_register(ParseInfo *info, Expr *offset, Operand *operand) {
  // Already read "(".
  enum RegType base_reg = find_register(&info->p);
  if (base_reg == NOREG) {
    parse_error(info, "register expected");
    return false;
  }
  if (*info->p != ')') {
    parse_error(info, "`)' expected");
    return false;
  }
  ++info->p;

  char no = base_reg - X0;
  operand->type = INDIRECT;
  operand->indirect.reg.no = no;
  operand->indirect.offset = offset;

  return true;
}

unsigned int parse_operand(ParseInfo *info, unsigned int opr_flag, Operand *operand) {
  if (opr_flag & RND) {
    enum RoundMode roundmode = find_round_mode(&info->p);
    if (roundmode != NOROUND) {
      operand->type = ROUNDMODE;
      operand->roundmode = roundmode;
      return RND;
    }
  }

  if (opr_flag & R64) {
    enum RegType reg = find_register(&info->p);
    if (reg != NOREG) {
      operand->type = REG;
      operand->reg.no = reg - X0;
      return R64;
    }
  }
  if (opr_flag & F64) {
    enum FRegType freg = find_fregister(&info->p);
    if (freg != NOFREG) {
      operand->type = FREG;
      operand->freg = freg;
      return F64;
    }
  }

  Expr *expr = parse_expr(info);
  if (opr_flag & IND) {
    if (*info->p == '(') {
      info->p += 1;
      if (parse_indirect_register(info, expr, operand))
        return IND;
    }
  }

  if (opr_flag & (IMM | EXP)) {
    if (expr != NULL) {
      if (expr->kind == EX_FIXNUM) {
        operand->type = IMMEDIATE;
        operand->immediate = expr->fixnum;
        return IMM;
      }
      operand->type = DIRECT;
      operand->direct.expr = expr;
      return EXP;
    }
  }

  return 0;
}

const ParseInstTable kParseInstTable[] = {
  [R_MV] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){MV, {R64, R64}} } },
  [R_LI] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){LI, {R64, IMM}} } },
  [R_LA] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){LA, {R64, EXP}} } },
  [R_ADD] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){ADD, {R64, R64, R64}} } },
  [R_ADDW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){ADDW, {R64, R64, R64}} } },
  [R_ADDI] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){ADDI, {R64, R64, IMM}} } },
  [R_ADDIW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){ADDIW, {R64, R64, IMM}} } },
  [R_SUB] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SUB, {R64, R64, R64}} } },
  [R_SUBW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SUBW, {R64, R64, R64}} } },
  [R_MUL] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){MUL, {R64, R64, R64}} } },
  [R_MULW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){MULW, {R64, R64, R64}} } },
  [R_DIV] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){DIV, {R64, R64, R64}} } },
  [R_DIVW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){DIVW, {R64, R64, R64}} } },
  [R_DIVU] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){DIVU, {R64, R64, R64}} } },
  [R_DIVUW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){DIVUW, {R64, R64, R64}} } },
  [R_REM] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){REM, {R64, R64, R64}} } },
  [R_REMW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){REMW, {R64, R64, R64}} } },
  [R_REMU] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){REMU, {R64, R64, R64}} } },
  [R_REMUW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){REMUW, {R64, R64, R64}} } },
  [R_AND] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){AND, {R64, R64, R64}} } },
  [R_ANDI] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){ANDI, {R64, R64, IMM}} } },
  [R_OR] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){OR, {R64, R64, R64}} } },
  [R_ORI] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){ORI, {R64, R64, IMM}} } },
  [R_XOR] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){XOR, {R64, R64, R64}} } },
  [R_XORI] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){XORI, {R64, R64, IMM}} } },
  [R_NEG] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){NEG, {R64, R64}} } },
  [R_NOT] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){NOT, {R64, R64}} } },
  [R_SEXT_B] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SEXT_B, {R64, R64}} } },
  [R_SEXT_H] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SEXT_H, {R64, R64}} } },
  [R_SEXT_W] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SEXT_W, {R64, R64}} } },
  [R_ZEXT_B] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){ZEXT_B, {R64, R64}} } },
  [R_ZEXT_H] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){ZEXT_H, {R64, R64}} } },
  [R_ZEXT_W] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){ZEXT_W, {R64, R64}} } },
  [R_SLL] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SLL, {R64, R64, R64}} } },
  [R_SLLI] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SLLI, {R64, R64, IMM}} } },
  [R_SLLIW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SLLIW, {R64, R64, IMM}} } },
  [R_SRL] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SRL, {R64, R64, R64}} } },
  [R_SRLI] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SRLI, {R64, R64, IMM}} } },
  [R_SRLIW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SRLIW, {R64, R64, IMM}} } },
  [R_SRA] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SRA, {R64, R64, R64}} } },
  [R_SRAI] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SRAI, {R64, R64, IMM}} } },
  [R_LB] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){LB, {R64, IND}} } },
  [R_LH] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){LH, {R64, IND}} } },
  [R_LW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){LW, {R64, IND}} } },
  [R_LD] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){LD, {R64, IND}} } },
  [R_LBU] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){LBU, {R64, IND}} } },
  [R_LHU] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){LHU, {R64, IND}} } },
  [R_LWU] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){LWU, {R64, IND}} } },
  [R_SB] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SB, {R64, IND}} } },
  [R_SH] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SH, {R64, IND}} } },
  [R_SW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SW, {R64, IND}} } },
  [R_SD] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SD, {R64, IND}} } },
  [R_SLT] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SLT, {R64, R64, R64}} } },
  [R_SLTI] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SLTI, {R64, R64, IMM}} } },
  [R_SLTU] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SLTU, {R64, R64, R64}} } },
  [R_SLTIU] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SLTIU, {R64, R64, IMM}} } },
  [R_SEQZ] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SEQZ, {R64, R64}} } },
  [R_SNEZ] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SNEZ, {R64, R64}} } },
  [R_SLTZ] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SLTZ, {R64, R64}} } },
  [R_SGTZ] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SGTZ, {R64, R64}} } },
  [R_J] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){J, {EXP}} } },
  [R_JR] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JR, {R64}} } },
  [R_JALR] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JALR, {R64}} } },
  [R_BEQ] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BEQ, {R64, R64, EXP}} } },
  [R_BNE] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BNE, {R64, R64, EXP}} } },
  [R_BLT] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BLT, {R64, R64, EXP}} } },
  [R_BGE] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BGE, {R64, R64, EXP}} } },
  [R_BLTU] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BLTU, {R64, R64, EXP}} } },
  [R_BGEU] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BGEU, {R64, R64, EXP}} } },
  [R_CALL] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){CALL, {EXP}} } },
  [R_RET] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){RET} } },
  [R_ECALL] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){ECALL} } },

  [R_FADD_D] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FADD_D, {F64, F64, F64}} } },
  [R_FADD_S] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FADD_S, {F64, F64, F64}} } },
  [R_FSUB_D] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FSUB_D, {F64, F64, F64}} } },
  [R_FSUB_S] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FSUB_S, {F64, F64, F64}} } },
  [R_FMUL_D] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FMUL_D, {F64, F64, F64}} } },
  [R_FMUL_S] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FMUL_S, {F64, F64, F64}} } },
  [R_FDIV_D] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FDIV_D, {F64, F64, F64}} } },
  [R_FDIV_S] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FDIV_S, {F64, F64, F64}} } },
  [R_FSQRT_D] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FSQRT_D, {F64, F64}} } },
  [R_FSQRT_S] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FSQRT_S, {F64, F64}} } },
  [R_FSGNJ_D] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FSGNJ_D, {F64, F64, F64}} } },
  [R_FSGNJ_S] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FSGNJ_S, {F64, F64, F64}} } },
  [R_FSGNJN_D] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FSGNJN_D, {F64, F64, F64}} } },
  [R_FSGNJN_S] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FSGNJN_S, {F64, F64, F64}} } },
  [R_FSGNJX_D] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FSGNJX_D, {F64, F64, F64}} } },
  [R_FSGNJX_S] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FSGNJX_S, {F64, F64, F64}} } },
  [R_FMV_D] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FMV_D, {F64, F64}} } },
  [R_FMV_S] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FMV_S, {F64, F64}} } },
  [R_FNEG_D] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FNEG_D, {F64, F64}} } },
  [R_FNEG_S] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FNEG_S, {F64, F64}} } },
  [R_FMV_X_D] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FMV_X_D, {R64, F64}} } },
  [R_FMV_X_W] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FMV_X_W, {R64, F64}} } },
  [R_FEQ_D] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FEQ_D, {R64, F64, F64}} } },
  [R_FEQ_S] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FEQ_S, {R64, F64, F64}} } },
  [R_FLT_D] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FLT_D, {R64, F64, F64}} } },
  [R_FLT_S] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FLT_S, {R64, F64, F64}} } },
  [R_FLE_D] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FLE_D, {R64, F64, F64}} } },
  [R_FLE_S] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FLE_S, {R64, F64, F64}} } },
  [R_FLD] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FLD, {F64, IND}} } },
  [R_FLW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FLW, {F64, IND}} } },
  [R_FSD] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FSD, {F64, IND}} } },
  [R_FSW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FSW, {F64, IND}} } },
  [R_FCVT_D_W] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FCVT_D_W, {F64, R64}} } },
  [R_FCVT_D_WU] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FCVT_D_WU, {F64, R64}} } },
  [R_FCVT_D_L] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FCVT_D_L, {F64, R64}} } },
  [R_FCVT_D_LU] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FCVT_D_LU, {F64, R64}} } },
  [R_FCVT_W_D] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FCVT_W_D, {R64, F64}},
    &(ParseOpArray){FCVT_W_D, {R64, F64, RND}},
  } },
  [R_FCVT_WU_D] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FCVT_WU_D, {R64, F64}},
    &(ParseOpArray){FCVT_WU_D, {R64, F64, RND}},
  } },
  [R_FCVT_L_D] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FCVT_L_D, {R64, F64}},
    &(ParseOpArray){FCVT_L_D, {R64, F64, RND}},
  } },
  [R_FCVT_LU_D] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FCVT_LU_D, {R64, F64}},
    &(ParseOpArray){FCVT_LU_D, {R64, F64, RND}},
  } },
  [R_FCVT_S_W] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FCVT_S_W, {F64, R64}} } },
  [R_FCVT_S_WU] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FCVT_S_WU, {F64, R64}} } },
  [R_FCVT_S_L] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FCVT_S_L, {F64, R64}} } },
  [R_FCVT_S_LU] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FCVT_S_LU, {F64, R64}} } },
  [R_FCVT_W_S] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FCVT_W_S, {R64, F64}},
    &(ParseOpArray){FCVT_W_S, {R64, F64, RND}},
  } },
  [R_FCVT_WU_S] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FCVT_WU_S, {R64, F64}},
    &(ParseOpArray){FCVT_WU_S, {R64, F64, RND}},
  } },
  [R_FCVT_L_S] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FCVT_L_S, {R64, F64}},
    &(ParseOpArray){FCVT_L_S, {R64, F64, RND}},
  } },
  [R_FCVT_LU_S] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FCVT_LU_S, {R64, F64}},
    &(ParseOpArray){FCVT_LU_S, {R64, F64, RND}},
  } },
  [R_FCVT_D_S] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FCVT_D_S, {F64, F64}} } },
  [R_FCVT_S_D] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FCVT_S_D, {F64, F64}} } },
};
