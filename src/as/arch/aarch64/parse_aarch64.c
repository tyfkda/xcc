#include "../../../config.h"
#include "parse_asm.h"

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <strings.h>

#include "util.h"

enum RawOpcode {
  R_NOOP,
  R_MOV,
  R_RET,
};

const char *kRawOpTable[] = {
  "mov",
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

static const RegisterTable kRegisters32[] = {
  {"w0", W0},    {"w1", W1},    {"w2", W2},    {"w3", W3},
  {"w4", W4},    {"w5", W5},    {"w6", W6},    {"w7", W7},
  {"w8", W8},    {"w9", W9},    {"w10", W10},  {"w11", W11},
  {"w12", W12},  {"w13", W13},  {"w14", W14},  {"w15", W15},
  {"w16", W16},  {"w17", W17},  {"w18", W18},  {"w19", W19},
  {"w20", W20},  {"w21", W21},  {"w22", W22},  {"w23", W23},
  {"w24", W24},  {"w25", W25},  {"w26", W26},  {"w27", W27},
  {"w28", W28},  {"w29", W29},  {"w30", W30},  {"w31", W31},
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
  {"fp", FP},    {"lr", LR},    {"sp", SP},
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

  return 0;
}

const ParseInstTable kParseInstTable[] = {
  [R_MOV] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){MOV, {R32 | R64, IMM}} } },
  [R_RET] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){RET} } },
};
