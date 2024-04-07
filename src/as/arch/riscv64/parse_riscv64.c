#include "../../../config.h"
#include "parse_asm.h"

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <strings.h>

#include "util.h"

// Align with Opcode.
static const char *kOpTable[] = {
  "li",
  "ret",
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
  {"x29", X29},
  {"x30", X30},
  {"x31", X31},

  // Alias
  {"zero", ZEROREG},
  {"ra", RA},
  {"sp", SP},
  {"gp", GP},
  {"tp", TP},
  {"t0", T0},
  {"t1", T1},
  {"t2", T2},
  {"fp", FP},
  {"s1", S1},
  {"a0", A0},
  {"a1", A1},
  {"a2", A2},
  {"a3", A3},
  {"a4", A4},
  {"a5", A5},
  {"a6", A6},
  {"a7", A7},
  {"s2", S2},
  {"s3", S3},
  {"s4", S4},
  {"s5", S5},
  {"s6", S6},
  {"s7", S7},
  {"s8", S8},
  {"s9", S9},
  {"s10", S10},
  {"s11", S11},
  {"t3", T3},
  {"t4", T4},
  {"t5", T5},
  {"t6", T6},
};

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
    if (strncmp(p, name, n) == 0) {
      *pp = p + n;
      return kRegisters[i].reg;
    }
  }
  return NOREG;
}

static bool parse_operand(ParseInfo *info, Operand *operand) {
  enum RegType reg = find_register(&info->p);
  if (reg != NOREG) {
    operand->type = REG;
    operand->reg.no = reg - ZEROREG;
    return true;
  }

  Expr *expr = parse_expr(info);
  if (expr != NULL) {
    if (expr->kind == EX_FIXNUM) {
      operand->type = IMMEDIATE;
      operand->immediate = expr->fixnum;
      return true;
    }
  }

  return false;
}

void parse_inst(ParseInfo *info, Inst *inst) {
  Operand *opr_table[] = {&inst->opr1, &inst->opr2, &inst->opr3};
  for (int i = 0; i < (int)ARRAY_SIZE(opr_table); ++i)
    opr_table[i]->type = NOOPERAND;

  enum Opcode op = find_opcode(info);
  inst->op = op;
  if (op != NOOP) {
    for (int i = 0; i < (int)ARRAY_SIZE(opr_table); ++i) {
      if (!parse_operand(info, opr_table[i]))
        break;
      info->p = skip_whitespaces(info->p);
      if (i == (int)ARRAY_SIZE(opr_table) - 1 || *info->p != ',')
        break;
      info->p = skip_whitespaces(info->p + 1);
    }
  }
}
