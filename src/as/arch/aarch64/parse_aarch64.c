#include "../../../config.h"
#include "parse_asm.h"

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <strings.h>

#include "util.h"

// Align with Opcode.
static const char *kOpTable[] = {
  "mov",
  "ret",
};

static const struct {
  const char *name;
  enum RegType reg;
} kRegisters[] = {
  {"w0", W0},
  {"w1", W1},
  {"w2", W2},
  {"w3", W3},
  {"w4", W4},
  {"w5", W5},
  {"w6", W6},
  {"w7", W7},
  {"w8", W8},
  {"w9", W9},
  {"w10", W10},
  {"w11", W11},
  {"w12", W12},
  {"w13", W13},
  {"w14", W14},
  {"w15", W15},
  {"w16", W16},
  {"w17", W17},
  {"w18", W18},
  {"w19", W19},
  {"w20", W20},
  {"w21", W21},
  {"w22", W22},
  {"w23", W23},
  {"w24", W24},
  {"w25", W25},
  {"w26", W26},
  {"w27", W27},
  {"w28", W28},
  {"w29", W29},
  {"w30", W30},
  {"w31", W31},
  {"x0", X0},
  {"x1", X1},
  {"x2", X2},
  {"x3", X3},
  {"x4", X4},
  {"x5", X5},
  {"x6", X6},
  {"x7", X7},
  {"x8", X8},
  {"x9", X9},
  {"x10", X10},
  {"x11", X11},
  {"x12", X12},
  {"x13", X13},
  {"x14", X14},
  {"x15", X15},
  {"x16", X16},
  {"x17", X17},
  {"x18", X18},
  {"x19", X19},
  {"x20", X20},
  {"x21", X21},
  {"x22", X22},
  {"x23", X23},
  {"x24", X24},
  {"x25", X25},
  {"x26", X26},
  {"x27", X27},
  {"x28", X28},
  {"fp", FP},
  {"lr", LR},
  {"sp", SP},
  // Alias
  {"x29", FP},
  {"x30", LR},
  {"x31", SP},
};

inline bool is_reg32(enum RegType reg) {
  return reg >= W0 && reg <= W31;
}

inline bool is_reg64(enum RegType reg) {
  return reg >= X0 && reg <= SP;
}

static int find_match_index(const char **pp, const char **table, size_t count) {
  const char *p = *pp;
  const char *start = p;

  while (isalnum(*p))
    ++p;
  if (*p == '\0' || isspace(*p)) {
    size_t n = p - start;
    for (size_t i = 0; i < count; ++i) {
      const char *name = table[i];
      size_t len = strlen(name);
      if (n == len && strncasecmp(start, name, n) == 0) {
        *pp = skip_whitespaces(p);
        return i;
      }
    }
  }
  return -1;
}

static enum Opcode find_opcode(ParseInfo *info) {
  return find_match_index(&info->p, kOpTable, ARRAY_SIZE(kOpTable)) + 1;
}

static enum RegType find_register(const char **pp) {
  const char *p = *pp;
  for (int i = 0; i < (int)ARRAY_SIZE(kRegisters); ++i) {
    const char *name = kRegisters[i].name;
    size_t n = strlen(name);
    if (strncmp(p, name, n) == 0 && !is_label_chr(p[n])) {
      *pp = p + n;
      return kRegisters[i].reg;
    }
  }
  return NOREG;
}

static bool parse_operand(ParseInfo *info, Operand *operand) {
  const char *p = info->p;
  if (*p == '#') {
    info->p = p + 1;
    if (!immediate(&info->p, &operand->immediate))
      parse_error(info, "Syntax error");
    operand->type = IMMEDIATE;
    return true;
  }

  enum RegType reg = find_register(&info->p);
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
      parse_error(info, "Illegal register");
      return false;
    }

    operand->type = REG;
    operand->reg.size = size;
    operand->reg.no = no;
    return true;
  }

  return false;
}

void parse_inst(ParseInfo *info, Line *line) {
  Inst *inst  = &line->inst;
  Operand *opr_table = inst->opr;
  for (int i = 0; i < (int)ARRAY_SIZE(inst->opr); ++i)
    opr_table[i].type = NOOPERAND;

  enum Opcode op = find_opcode(info);
  inst->op = op;
  if (op != NOOP) {
    for (int i = 0; i < (int)ARRAY_SIZE(inst->opr); ++i) {
      if (!parse_operand(info, &opr_table[i]))
        break;
      info->p = skip_whitespaces(info->p);
      if (i == (int)ARRAY_SIZE(inst->opr) - 1 || *info->p != ',')
        break;
      info->p = skip_whitespaces(info->p + 1);
    }
  }
}
