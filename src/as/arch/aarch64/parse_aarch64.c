#include "../../../config.h"
#include "parse_asm.h"

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <strings.h>

#include "util.h"

enum RawOpcode {
  R_NOOP,
  R_MOV, R_MOVK,
  R_ADD, R_SUB,
  R_MUL, R_SDIV, R_UDIV,
  R_MADD, R_MSUB,
  R_AND, R_ORR, R_EOR, R_EON,
  R_CMP, R_CMN,
  R_LSL, R_LSR, R_ASR,
  R_SXTB, R_SXTH, R_SXTW,
  R_UXTB, R_UXTH, R_UXTW,
  R_LDRB, R_LDRH, R_LDR, R_LDRSB, R_LDRSH, R_LDRSW,
  R_STRB, R_STRH, R_STR,
  R_LDP, R_STP,
  R_ADRP,
  R_CSET,
  R_B, R_BR,
  R_BEQ, R_BNE, R_BHS, R_BLO, R_BMI, R_BPL, R_BVS, R_BVC,
  R_BHI, R_BLS, R_BGE, R_BLT, R_BGT, R_BLE, R_BAL, R_BNV,
  R_BL, R_BLR,
  R_RET,
  R_SVC,

  R_FMOV,
  R_FADD, R_FSUB, R_FMUL, R_FDIV,
  R_FCMP, R_FNEG,
  R_FSQRT,
  R_SCVTF, R_UCVTF,
  R_FCVT, R_FCVTZS, R_FCVTZU,
};

const char *kRawOpTable[] = {
  "mov", "movk",
  "add", "sub", "mul", "sdiv", "udiv",
  "madd", "msub",
  "and", "orr", "eor", "eon",
  "cmp", "cmn",
  "lsl", "lsr", "asr",
  "sxtb", "sxth", "sxtw",
  "uxtb", "uxth", "uxtw",
  "ldrb", "ldrh", "ldr", "ldrsb", "ldrsh", "ldrsw",
  "strb", "strh", "str",
  "ldp", "stp",
  "adrp",
  "cset",
  "b", "br",
  "beq", "bne", "bhs", "blo", "bmi", "bpl", "bvs", "bvc",
  "bhi", "bls", "bge", "blt", "bgt", "ble", "bal", "bnv",
  "bl", "blr",
  "ret",
  "svc",

  "fmov",
  "fadd", "fsub", "fmul", "fdiv",
  "fcmp", "fneg",
  "fsqrt",
  "scvtf", "ucvtf",
  "fcvt", "fcvtzs", "fcvtzu",
  NULL,
};

enum RegType {
  NOREG = -1,

  // 32bit
   W0,  W1,  W2,  W3,  W4,  W5,  W6,  W7,  W8,  W9, W10, W11, W12, W13, W14, W15,
  W16, W17, W18, W19, W20, W21, W22, W23, W24, W25, W26, W27, W28, W29, W30, WZR,

  // 64bit
   X0,  X1,  X2,  X3,  X4,  X5,  X6,  X7,  X8,  X9, X10, X11, X12, X13, X14, X15,
  X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, XZR,
  SP,

  // FP32bit
    S0,  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9, S10, S11, S12, S13, S14, S15,
   S16, S17, S18, S19, S20, S21, S22, S23, S24, S25, S26, S27, S28, S29, S30, S31,

  // FP64bit
    D0,  D1,  D2,  D3,  D4,  D5,  D6,  D7,  D8,  D9, D10, D11, D12, D13, D14, D15,
   D16, D17, D18, D19, D20, D21, D22, D23, D24, D25, D26, D27, D28, D29, D30, D31,
};

typedef struct {
  const char *name;
  enum RegType reg;
} RegisterTable;

#define FP   X29
#define LR   X30

static const RegisterTable kRegisters32[] = {
  {"w0", W0},    {"w1", W1},    {"w2", W2},    {"w3", W3},
  {"w4", W4},    {"w5", W5},    {"w6", W6},    {"w7", W7},
  {"w8", W8},    {"w9", W9},    {"w10", W10},  {"w11", W11},
  {"w12", W12},  {"w13", W13},  {"w14", W14},  {"w15", W15},
  {"w16", W16},  {"w17", W17},  {"w18", W18},  {"w19", W19},
  {"w20", W20},  {"w21", W21},  {"w22", W22},  {"w23", W23},
  {"w24", W24},  {"w25", W25},  {"w26", W26},  {"w27", W27},
  {"w28", W28},  {"w29", W29},  {"w30", W30},  {"wzr", WZR},
};

static const RegisterTable kRegisters64[] = {
  {"x0", X0},    {"x1", X1},    {"x2", X2},    {"x3", X3},
  {"x4", X4},    {"x5", X5},    {"x6", X6},    {"x7", X7},
  {"x8", X8},    {"x9", X9},    {"x10", X10},  {"x11", X11},
  {"x12", X12},  {"x13", X13},  {"x14", X14},  {"x15", X15},
  {"x16", X16},  {"x17", X17},  {"x18", X18},  {"x19", X19},
  {"x20", X20},  {"x21", X21},  {"x22", X22},  {"x23", X23},
  {"x24", X24},  {"x25", X25},  {"x26", X26},  {"x27", X27},
  {"x28", X28},  {"x29", X29},  {"x30", X30},  {"xzr", XZR},
  // Alias
  {"fp", FP},    {"lr", LR},
  // Stack pointer
  {"sp", SP},
};

static const RegisterTable kFRegisters32[] = {
  {"s0", S0},    {"s1", S1},    {"s2", S2},    {"s3", S3},
  {"s4", S4},    {"s5", S5},    {"s6", S6},    {"s7", S7},
  {"s8", S8},    {"s9", S9},    {"s10", S10},  {"s11", S11},
  {"s12", S12},  {"s13", S13},  {"s14", S14},  {"s15", S15},
  {"s16", S16},  {"s17", S17},  {"s18", S18},  {"s19", S19},
  {"s20", S20},  {"s21", S21},  {"s22", S22},  {"s23", S23},
  {"s24", S24},  {"s25", S25},  {"s26", S26},  {"s27", S27},
  {"s28", S28},  {"s29", S29},  {"s30", S30},  {"s31", S31},
};

static const RegisterTable kFRegisters64[] = {
  {"d0", D0},    {"d1", D1},    {"d2", D2},    {"d3", D3},
  {"d4", D4},    {"d5", D5},    {"d6", D6},    {"d7", D7},
  {"d8", D8},    {"d9", D9},    {"d10", D10},  {"d11", D11},
  {"d12", D12},  {"d13", D13},  {"d14", D14},  {"d15", D15},
  {"d16", D16},  {"d17", D17},  {"d18", D18},  {"d19", D19},
  {"d20", D20},  {"d21", D21},  {"d22", D22},  {"d23", D23},
  {"d24", D24},  {"d25", D25},  {"d26", D26},  {"d27", D27},
  {"d28", D28},  {"d29", D29},  {"d30", D30},  {"d31", D31},
};

static const char kCondTable[][3] = {
  "eq", "ne", "hs", "lo", "mi", "pl", "vs", "vc",
  "hi", "ls", "ge", "lt", "gt", "le", "al", "nv",
};

inline bool is_reg32(enum RegType reg) {
  return reg >= W0 && reg <= WZR;
}

inline bool is_reg64(enum RegType reg) {
  return reg >= X0 && reg <= SP;
}

inline bool is_freg32(enum RegType reg) {
  return reg >= S0 && reg <= S31;
}

inline bool is_freg64(enum RegType reg) {
  return reg >= D0 && reg <= D31;
}

#define R32  (1 << 0)
#define R64  (1 << 1)
#define F32  (1 << 2)
#define F64  (1 << 3)
#define RSP  (1 << 4)
#define RZR  (1 << 5)
#define IMM  (1 << 6)
#define IND  (1 << 7)
#define ROI  (1 << 8)  // Register Offset Indirect
#define EXP  (1 << 9)
#define CND  (1 << 10)
#define SFT  (1 << 11)  // lsl #nn
#define EXT  (1 << 12)  // UXTB, UXTH, UXTW, UXTX, SXTB, SXTH, SXTW, SXTX, LSL, LSR, ASR

static enum RegType find_register(const char **pp, unsigned int flag) {
  const char *p = *pp;
  static const RegisterTable *kRegisters[] = { kRegisters32, kRegisters64, kFRegisters32, kFRegisters64 };
  static const int kRegistersCount[] = { ARRAY_SIZE(kRegisters32), ARRAY_SIZE(kRegisters64), ARRAY_SIZE(kFRegisters32), ARRAY_SIZE(kFRegisters64) };
  for (int i = 0; i < (int)ARRAY_SIZE(kRegisters); ++i) {
    if ((flag & (R32 << i)) == 0)
      continue;

    const RegisterTable *regs = kRegisters[i];
    for (int j = 0, n = kRegistersCount[i]; j < n; ++j) {
      const char *name = regs[j].name;
      size_t n = strlen(name);
      if (strncasecmp(p, name, n) == 0 && !is_label_chr(p[n])) {
        *pp = p + n;
        return regs[j].reg;
      }
    }
  }
  return NOREG;
}

static enum CondType find_cond(const char **pp) {
  const char *p = *pp;
  for (int i = 0; i < (int)ARRAY_SIZE(kCondTable); ++i) {
    const char *name = kCondTable[i];
    size_t n = strlen(name);
    if (strncasecmp(p, name, n) == 0 && !is_label_chr(p[n])) {
      *pp = p + n;
      return i;
    }
  }
  return NOCOND;
}

static unsigned int parse_indirect_register(ParseInfo *info, Operand *operand) {
  const char *p = skip_whitespaces(info->p);
  enum RegType reg2 = NOREG;
  int extend = 0;
  enum RegType reg = find_register(&p, R64);
  if (reg == NOREG) {
    parse_error(info, "Base register expected");
    return 0;
  }
  if (reg == SP) {
    operand->indirect.reg.size = REG64;
    operand->indirect.reg.no = 31;
  } else if (is_reg64(reg) && reg != XZR) {
    operand->indirect.reg.size = REG64;
    operand->indirect.reg.no = reg - X0;
  } else {
    parse_error(info, "Base register expected");
  }

  Expr *offset = NULL, *scale = NULL;
  int prepost = 0;
  p = skip_whitespaces(p);
  if (*p == ',') {
    p = skip_whitespaces(p + 1);
    if (*p == '#') {
      ++p;
      int64_t imm;
      if (immediate(&p, &imm)) {
        offset = new_expr(EX_FIXNUM);
        offset->fixnum = imm;
      } else {
        parse_set_p(info, p);
        offset = parse_got_label(info);
        if (offset != NULL) {
          p = info->p;
        } else {
          parse_error(info, "Offset expected");
        }
      }
    } else {
      reg2 = find_register(&p, R32 | R64);
      if (reg2 == NOREG)
        return 0;  // Error

      p = skip_whitespaces(p);
      if (*p == ',') {
        p = skip_whitespaces(p + 1);
        static const char kExtendTable[][5] = { "sxtw", "uxtw", "lsl", "sxtx" };
        for (int i = 0; i < (int)ARRAY_SIZE(kExtendTable); ++i) {
          const char *name = kExtendTable[i];
          size_t n = strlen(name);
          if (strncasecmp(p, name, n) == 0 && isspace(p[n])) {
            extend = i + 1;
            p = skip_whitespaces(p + n + 1);

            if (*p == '#') {
              p = p + 1;
              int64_t imm;
              if (immediate(&p, &imm)) {
                scale = new_expr(EX_FIXNUM);
                scale->fixnum = imm;
              } else {
                // parse_error(info, "Offset expected");
                return 0;  // Error
              }
            }
            break;
          }
        }
      }
    }

    p = skip_whitespaces(p);
  }

  if (*p != ']')
    // parse_error(info, "`]' expected");
    return 0;  // Error

  p = skip_whitespaces(p + 1);
  if (reg2 == NOREG) {
    if (offset != NULL) {
      if (*p == '!') {
        prepost = 1;
        ++p;
      }
    } else if (*p == ',') {
      const char *q = skip_whitespaces(p + 1);
      if (*q == '#') {
        p = q + 1;
        int64_t imm;
        if (immediate(&p, &imm)) {
          offset = new_expr(EX_FIXNUM);
          offset->fixnum = imm;
          prepost = 2;
        } else {
          // parse_error(info, "Offset expected");
          return 0;  // Error
        }
      }
    }

    operand->type = INDIRECT;
    operand->indirect.offset = offset;
    operand->indirect.prepost = prepost;
    parse_set_p(info, p);
    return IND;
  } else {
    operand->type = REGISTER_OFFSET;
    operand->register_offset.base_reg.size = REG64;
    operand->register_offset.base_reg.no = reg - X0;
    if (is_reg64(reg2)) {
      operand->register_offset.index_reg.size = REG64;
      operand->register_offset.index_reg.no = reg2 - X0;
    } else {
      operand->register_offset.index_reg.size = REG32;
      operand->register_offset.index_reg.no = reg2 - W0;
    }
    operand->register_offset.extend = extend;
    operand->register_offset.scale = scale;
    parse_set_p(info, p);
    return ROI;
  }
}

static bool parse_extend(ParseInfo *info, Operand *operand) {
  static const char table[][5] = {
    "uxtb",
    "uxth",
    "uxtw",
    "uxtx",
    "sxtb",
    "sxth",
    "sxtw",
    "sxtx",
    "lsl",
    "lsr",
    "asr",
  };

  const char *p = info->p;
  for (int i = 0; i < (int)ARRAY_SIZE(table); ++i) {
    const char *ex = table[i];
    size_t n = strlen(ex);
    if (strncasecmp(p, ex, n) == 0 && !is_label_chr(p[n])) {
      p += n;
      operand->type = EXTEND;
      operand->extend.option = i;
      operand->extend.imm = 0;
      int64_t imm = 0;
      if (isspace(*p) && (p = skip_whitespaces(p), *p == '#')) {
        ++p;
        if (!immediate(&p, &imm))
          parse_error(info, "immediate value expected");
      } else if (i >= 8) {
        parse_error(info, "immediate value for shift expected");
      }
      operand->extend.imm = imm;
      info->p = p;
      return true;
    }
  }
  return false;
}

unsigned int parse_operand(ParseInfo *info, unsigned int opr_flag, Operand *operand) {
  const char *p = info->p;
  if (opr_flag & IMM) {
    if (*p == '#') {
      info->p = p + 1;
      if (!immediate(&info->p, &operand->immediate))
        return 0;
      operand->type = IMMEDIATE;
      return IMM;
    }
  }

  if (opr_flag & IND) {
    if (*p == '[') {
      info->p = p + 1;
      return parse_indirect_register(info, operand);
    }
  }

  if (opr_flag & (R32 | R64 | F32 | F64)) {
    enum RegType reg = find_register(&info->p, opr_flag);
    if (reg != NOREG) {
      enum RegSize size;
      int no;
      unsigned int result;
      if (reg == SP) {
        size = REG64;
        no = 31;
        result = RSP;
      } else if (is_reg32(reg)) {
        size = REG32;
        no = reg - W0;
        result = R32;
      } else if (is_reg64(reg)) {
        size = REG64;
        no = reg - X0;
        result = R64;
      } else if (is_freg32(reg)) {
        size = REG32;
        no = reg - S0;
        result = F32;
      } else if (is_freg64(reg)) {
        size = REG64;
        no = reg - D0;
        result = F64;
      } else {
        assert(false);
        return 0;
      }

      operand->type = (result & (F32 | F64)) != 0 ? FREG : REG;
      operand->reg.size = size;
      operand->reg.no = no;
      operand->reg.sp = reg == SP;
      return result;
    }
  }

  if (opr_flag & CND) {
    enum CondType cond = find_cond(&info->p);
    if (cond != NOCOND) {
      operand->type = COND;
      operand->cond = cond;
      return CND;
    }
  }

  if (opr_flag & SFT) {
    if (strncasecmp(p, "lsl #", 5) == 0) {
      p += 5;
      int64_t imm;
      if (immediate(&p, &imm) && imm >= 0 && imm <= 48 && (imm & 15) == 0) {
        info->p = p;
        operand->type = SHIFT;
        operand->immediate = imm;
        return SFT;
      }
    }
  }

  if (opr_flag & EXT) {
    info->p = p;
    if (parse_extend(info, operand)) {
      return EXT;
    }
  }

  if (opr_flag & EXP) {
    Expr *expr = parse_expr(info);
    if (expr != NULL) {
      operand->type = DIRECT;
      operand->direct.expr = expr;
      return EXP;
    }
  }

  return 0;
}

const ParseInstTable kParseInstTable[] = {
  [R_MOV] = { 3, (const ParseOpArray*[]){
    &(ParseOpArray){MOV, {R32, R32}},
    &(ParseOpArray){MOV, {R64 | RSP, R64 | RSP}},
    &(ParseOpArray){MOV, {R32 | R64, IMM}},
  } },
  [R_MOVK] = { 1, (const ParseOpArray*[]){
    &(ParseOpArray){MOVK, {R32 | R64, IMM, SFT}},
  } },
  [R_ADD] = { 9, (const ParseOpArray*[]){
    &(ParseOpArray){ADD_R, {R32, R32, R32}},
    &(ParseOpArray){ADD_R, {R32, R32, R32, EXT}},
    &(ParseOpArray){ADD_I, {R32, R32, IMM}},
    &(ParseOpArray){ADD_I, {R32, R32, EXP}},
    &(ParseOpArray){ADD_R, {R64, R64, R64}},
    &(ParseOpArray){ADD_R, {R64, R64, R64, EXT}},
    &(ParseOpArray){ADD_R, {R64 | RSP, R64 | RSP, R64}},
    &(ParseOpArray){ADD_I, {R64 | RSP, R64 | RSP, IMM}},
    &(ParseOpArray){ADD_I, {R64 | RSP, R64 | RSP, EXP}},
  } },
  [R_SUB] = { 9, (const ParseOpArray*[]){
    &(ParseOpArray){SUB_R, {R32, R32, R32}},
    &(ParseOpArray){SUB_R, {R32, R32, R32, EXT}},
    &(ParseOpArray){SUB_I, {R32, R32, IMM}},
    &(ParseOpArray){SUB_I, {R32, R32, EXP}},
    &(ParseOpArray){SUB_R, {R64, R64, R64}},
    &(ParseOpArray){SUB_R, {R64, R64, R64, EXT}},
    &(ParseOpArray){SUB_R, {R64 | RSP, R64 | RSP, R64}},
    &(ParseOpArray){SUB_I, {R64 | RSP, R64 | RSP, IMM}},
    &(ParseOpArray){SUB_I, {R64 | RSP, R64 | RSP, EXP}},
  } },
  [R_MUL] = { 2, (const ParseOpArray*[]){ &(ParseOpArray){MUL, {R32, R32, R32}}, &(ParseOpArray){MUL, {R64, R64, R64}} } },
  [R_SDIV] = { 2, (const ParseOpArray*[]){ &(ParseOpArray){SDIV, {R32, R32, R32}}, &(ParseOpArray){SDIV, {R64, R64, R64}} } },
  [R_UDIV] = { 2, (const ParseOpArray*[]){ &(ParseOpArray){UDIV, {R32, R32, R32}}, &(ParseOpArray){UDIV, {R64, R64, R64}} } },
  [R_MADD] = { 2, (const ParseOpArray*[]){ &(ParseOpArray){MADD, {R32, R32, R32, R32}}, &(ParseOpArray){MADD, {R64, R64, R64, R64}} } },
  [R_MSUB] = { 2, (const ParseOpArray*[]){ &(ParseOpArray){MSUB, {R32, R32, R32, R32}}, &(ParseOpArray){MSUB, {R64, R64, R64, R64}} } },
  [R_AND] = { 2, (const ParseOpArray*[]){ &(ParseOpArray){AND, {R32, R32, R32}}, &(ParseOpArray){AND, {R64, R64, R64}} } },
  [R_ORR] = { 2, (const ParseOpArray*[]){ &(ParseOpArray){ORR, {R32, R32, R32}}, &(ParseOpArray){ORR, {R64, R64, R64}} } },
  [R_EOR] = { 2, (const ParseOpArray*[]){ &(ParseOpArray){EOR, {R32, R32, R32}}, &(ParseOpArray){EOR, {R64, R64, R64}} } },
  [R_EON] = { 2, (const ParseOpArray*[]){ &(ParseOpArray){EON, {R32, R32, R32}}, &(ParseOpArray){EON, {R64, R64, R64}} } },
  [R_CMP] = { 3, (const ParseOpArray*[]){
    &(ParseOpArray){CMP_R, {R32, R32}},
    &(ParseOpArray){CMP_R, {R64, R64}},
    &(ParseOpArray){CMP_I, {R32 | R64, IMM}},
  } },
  [R_CMN] = { 3, (const ParseOpArray*[]){
    &(ParseOpArray){CMN_R, {R32, R32}},
    &(ParseOpArray){CMN_R, {R64, R64}},
    &(ParseOpArray){CMN_I, {R32 | R64, IMM}},
  } },
  [R_LSL] = { 4, (const ParseOpArray*[]){
    &(ParseOpArray){LSL_R, {R32, R32, R32}},
    &(ParseOpArray){LSL_I, {R32, R32, IMM}},
    &(ParseOpArray){LSL_R, {R64, R64, R64}},
    &(ParseOpArray){LSL_I, {R64, R64, IMM}},
  } },
  [R_LSR] = { 4, (const ParseOpArray*[]){
    &(ParseOpArray){LSR_R, {R32, R32, R32}},
    &(ParseOpArray){LSR_I, {R32, R32, IMM}},
    &(ParseOpArray){LSR_R, {R64, R64, R64}},
    &(ParseOpArray){LSR_I, {R64, R64, IMM}},
  } },
  [R_ASR] = { 4, (const ParseOpArray*[]){
    &(ParseOpArray){ASR_R, {R32, R32, R32}},
    &(ParseOpArray){ASR_I, {R32, R32, IMM}},
    &(ParseOpArray){ASR_R, {R64, R64, R64}},
    &(ParseOpArray){ASR_I, {R64, R64, IMM}},
  } },
  [R_SXTB] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SXTB, {R32 | R64, R32}} } },
  [R_SXTH] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SXTH, {R32 | R64, R32}} } },
  [R_SXTW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SXTW, {R64, R32}} } },
  [R_UXTB] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){UXTB, {R32 | R64, R32}} } },
  [R_UXTH] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){UXTH, {R32 | R64, R32}} } },
  [R_UXTW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){UXTW, {R64, R32}} } },
  [R_LDRB] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){LDRB, {R32 | R64, IND | ROI}} } },
  [R_LDRH] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){LDRH, {R32 | R64, IND | ROI}} } },
  [R_LDR] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){LDR, {R32 | R64, IND | ROI}},
    &(ParseOpArray){F_LDR, {F32 | F64, IND | ROI}},
  } },
  [R_LDRSB] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){LDRSB, {R32 | R64, IND | ROI}} } },
  [R_LDRSH] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){LDRSH, {R32 | R64, IND | ROI}} } },
  [R_STRB] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){STRB, {R32, IND | ROI}} } },
  [R_STRH] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){STRH, {R32, IND | ROI}} } },
  [R_STR] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){STR, {R32 | R64, IND | ROI}},
    &(ParseOpArray){F_STR, {F32 | F64, IND | ROI}},
  } },
  [R_LDP] = { 4, (const ParseOpArray*[]){
    &(ParseOpArray){LDP, {R32, R32, IND}},
    &(ParseOpArray){LDP, {R64, R64, IND}},
    &(ParseOpArray){F_LDP, {F32, F32, IND}},
    &(ParseOpArray){F_LDP, {F64, F64, IND}},
  } },
  [R_STP] = { 4, (const ParseOpArray*[]){
    &(ParseOpArray){STP, {R32, R32, IND}},
    &(ParseOpArray){STP, {R64, R64, IND}},
    &(ParseOpArray){F_STP, {F32, F32, IND}},
    &(ParseOpArray){F_STP, {F64, F64, IND}},
  } },
  [R_ADRP] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){ADRP, {R64, EXP}} } },
  [R_CSET] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){CSET, {R32 | R64, CND}} } },
  [R_B] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){B, {EXP}} } },
  [R_BR] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BR, {R64}} } },
  [R_BEQ] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BEQ, {EXP}} } },
  [R_BNE] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BNE, {EXP}} } },
  [R_BHS] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BHS, {EXP}} } },
  [R_BLO] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BLO, {EXP}} } },
  [R_BMI] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BMI, {EXP}} } },
  [R_BPL] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BPL, {EXP}} } },
  [R_BVS] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BVS, {EXP}} } },
  [R_BVC] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BVC, {EXP}} } },
  [R_BHI] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BHI, {EXP}} } },
  [R_BLS] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BLS, {EXP}} } },
  [R_BGE] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BGE, {EXP}} } },
  [R_BLT] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BLT, {EXP}} } },
  [R_BGT] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BGT, {EXP}} } },
  [R_BLE] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BLE, {EXP}} } },
  [R_BAL] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BAL, {EXP}} } },
  [R_BNV] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BNV, {EXP}} } },
  [R_BL] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BL, {EXP}} } },
  [R_BLR] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){BLR, {R64}} } },
  [R_RET] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){RET} } },
  [R_SVC] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SVC, {IMM}} } },

  [R_FMOV] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FMOV, {F32, F32}},
    &(ParseOpArray){FMOV, {F64, F64}},
  } },
  [R_FADD] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FADD, {F32, F32, F32}},
    &(ParseOpArray){FADD, {F64, F64, F64}},
  } },
  [R_FSUB] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FSUB, {F32, F32, F32}},
    &(ParseOpArray){FSUB, {F64, F64, F64}},
  } },
  [R_FMUL] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FMUL, {F32, F32, F32}},
    &(ParseOpArray){FMUL, {F64, F64, F64}},
  } },
  [R_FDIV] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FDIV, {F32, F32, F32}},
    &(ParseOpArray){FDIV, {F64, F64, F64}},
  } },
  [R_FCMP] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FCMP, {F32, F32}},
    &(ParseOpArray){FCMP, {F64, F64}},
  } },
  [R_FNEG] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FNEG, {F32, F32}},
    &(ParseOpArray){FNEG, {F64, F64}},
  } },
  [R_FSQRT] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FSQRT, {F32, F32}},
    &(ParseOpArray){FSQRT, {F64, F64}},
  } },
  [R_SCVTF] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SCVTF, {F32 | F64, R32 | R64}} } },
  [R_UCVTF] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){UCVTF, {F32 | F64, R32 | R64}} } },
  [R_FCVT] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){FCVT, {F64, F32}},
    &(ParseOpArray){FCVT, {F32, F64}},
  } },
  [R_FCVTZS] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FCVTZS, {R32 | R64, F32 | F64}} } },
  [R_FCVTZU] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){FCVTZU, {R32 | R64, F32 | F64}} } },
};
