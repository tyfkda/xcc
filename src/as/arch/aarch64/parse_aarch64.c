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
  NULL,
};

enum RegType {
  NOREG = -1,

  // 32bit
   W0,  W1,  W2,  W3,  W4,  W5,  W6,  W7,  W8,  W9, W10, W11, W12, W13, W14, W15,
  W16, W17, W18, W19, W20, W21, W22, W23, W24, W25, W26, W27, W28, W29, W30, W31,

  // 64bit
   X0,  X1,  X2,  X3,  X4,  X5,  X6,  X7,  X8,  X9, X10, X11, X12, X13, X14, X15,
  X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31,
};

typedef struct {
  const char *name;
  enum RegType reg;
} RegisterTable;

#define FP   X29
#define LR   X30
#define SP   X31
#define XZR  X31
#define WZR  W31

static const RegisterTable kRegisters32[] = {
  {"w0", W0},    {"w1", W1},    {"w2", W2},    {"w3", W3},
  {"w4", W4},    {"w5", W5},    {"w6", W6},    {"w7", W7},
  {"w8", W8},    {"w9", W9},    {"w10", W10},  {"w11", W11},
  {"w12", W12},  {"w13", W13},  {"w14", W14},  {"w15", W15},
  {"w16", W16},  {"w17", W17},  {"w18", W18},  {"w19", W19},
  {"w20", W20},  {"w21", W21},  {"w22", W22},  {"w23", W23},
  {"w24", W24},  {"w25", W25},  {"w26", W26},  {"w27", W27},
  {"w28", W28},  {"w29", W29},  {"w30", W30},  {"w31", W31},
  // Alias
  {"wzr", WZR},
};

static const RegisterTable kRegisters64[] = {
  {"x0", X0},    {"x1", X1},    {"x2", X2},    {"x3", X3},
  {"x4", X4},    {"x5", X5},    {"x6", X6},    {"x7", X7},
  {"x8", X8},    {"x9", X9},    {"x10", X10},  {"x11", X11},
  {"x12", X12},  {"x13", X13},  {"x14", X14},  {"x15", X15},
  {"x16", X16},  {"x17", X17},  {"x18", X18},  {"x19", X19},
  {"x20", X20},  {"x21", X21},  {"x22", X22},  {"x23", X23},
  {"x24", X24},  {"x25", X25},  {"x26", X26},  {"x27", X27},
  {"x28", X28},  {"x29", X29},  {"x30", X30},  {"x31", X31},
  // Alias
  {"fp", FP},    {"lr", LR},    {"sp", SP},    {"xzr", XZR},
};

static const char kCondTable[][3] = {
  "eq", "ne", "hs", "lo", "mi", "pl", "vs", "vc",
  "hi", "ls", "ge", "lt", "gt", "le", "al", "nv",
};

inline bool is_reg32(enum RegType reg) {
  return reg >= W0 && reg <= W31;
}

inline bool is_reg64(enum RegType reg) {
  return reg >= X0 && reg <= SP;
}

#define R32  (1 << 0)
#define R64  (1 << 1)
#define IMM  (1 << 2)
#define IND  (1 << 3)
#define ROI  (1 << 4)  // Register Offset Indirect
#define EXP  (1 << 5)
#define CND  (1 << 6)
#define SFT  (1 << 7)  // lsl #nn

static enum RegType find_register(const char **pp, unsigned int flag) {
  const char *p = *pp;
  static const RegisterTable *kRegisters[] = { kRegisters32, kRegisters64 };
  static const int kRegistersCount[] = { ARRAY_SIZE(kRegisters32), ARRAY_SIZE(kRegisters64) };
  for (int i = 0; i < (int)ARRAY_SIZE(kRegisters); ++i) {
    if ((flag & (R32 << i)) == 0)
      continue;

    const RegisterTable *regs = kRegisters[i];
    for (int j = 0, n = kRegistersCount[i]; j < n; ++j) {
      const char *name = regs[j].name;
      size_t n = strlen(name);
      if (strncmp(p, name, n) == 0 && !is_label_chr(p[n])) {
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
    if (strncmp(p, name, n) == 0 && !is_label_chr(p[n])) {
      *pp = p + n;
      return i;
    }
  }
  return NOCOND;
}

static unsigned int parse_indirect_register(ParseInfo *info, Operand *operand) {
  const char *p = skip_whitespaces(info->p);
  enum RegType reg2 = NOREG;
  enum ExtendType extend = NOEXTEND;
  enum RegType reg = find_register(&p, R64);
  if (reg == NOREG) {
    parse_error(info, "Base register expected");
    return 0;
  }
  if (is_reg64(reg)) {
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
          if (strncmp(p, name, n) == 0 && isspace(p[n])) {
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

  if (opr_flag & (R32 | R64)) {
    enum RegType reg = find_register(&info->p, opr_flag);
    if (reg != NOREG) {
      enum RegSize size;
      int no;
      if (is_reg32(reg)) {
        size = REG32;
        no = reg - W0;
      } else if (is_reg64(reg)) {
        size = REG64;
        no = reg - X0;
      } else {
        assert(false);
        return 0;
      }

      operand->type = REG;
      operand->reg.size = size;
      operand->reg.no = no;
      return size == REG32 ? R32 : R64;
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
    if (strncmp(p, "lsl #", 5) == 0) {
      p += 5;
      int64_t imm;
      if (immediate(&p, &imm) && imm >= 0 && imm <= 48 && (imm & 15) == 0) {
        info->p = p;
        operand->type = IMMEDIATE;
        operand->immediate = imm;
        return SFT;
      }
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
    &(ParseOpArray){MOV, {R64, R64}},
    &(ParseOpArray){MOV, {R32 | R64, IMM}},
  } },
  [R_MOVK] = { 1, (const ParseOpArray*[]){
    &(ParseOpArray){MOVK, {R32 | R64, IMM, SFT}},
  } },
  [R_ADD] = { 6, (const ParseOpArray*[]){
    &(ParseOpArray){ADD_R, {R32, R32, R32}},
    &(ParseOpArray){ADD_I, {R32, R32, IMM}},
    &(ParseOpArray){ADD_I, {R32, R32, EXP}},
    &(ParseOpArray){ADD_R, {R64, R64, R64}},
    &(ParseOpArray){ADD_I, {R64, R64, IMM}},
    &(ParseOpArray){ADD_I, {R64, R64, EXP}},
  } },
  [R_SUB] = { 6, (const ParseOpArray*[]){
    &(ParseOpArray){SUB_R, {R32, R32, R32}},
    &(ParseOpArray){SUB_I, {R32, R32, IMM}},
    &(ParseOpArray){SUB_I, {R32, R32, EXP}},
    &(ParseOpArray){SUB_R, {R64, R64, R64}},
    &(ParseOpArray){SUB_I, {R64, R64, IMM}},
    &(ParseOpArray){SUB_I, {R64, R64, EXP}},
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
  [R_LDR] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){LDR, {R32 | R64, IND | ROI}} } },
  [R_LDRSB] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){LDRSB, {R32 | R64, IND | ROI}} } },
  [R_LDRSH] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){LDRSH, {R32 | R64, IND | ROI}} } },
  [R_STRB] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){STRB, {R32, IND | ROI}} } },
  [R_STRH] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){STRH, {R32, IND | ROI}} } },
  [R_STR] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){STR, {R32 | R64, IND | ROI}} } },
  [R_LDP] = { 2, (const ParseOpArray*[]){ &(ParseOpArray){LDP, {R32, R32, IND}}, &(ParseOpArray){LDP, {R64, R64, IND}} } },
  [R_STP] = { 2, (const ParseOpArray*[]){ &(ParseOpArray){STP, {R32, R32, IND}}, &(ParseOpArray){STP, {R64, R64, IND}} } },
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
};
