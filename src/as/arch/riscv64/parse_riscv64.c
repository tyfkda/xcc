#include "../../../config.h"
#include "parse_asm.h"

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <strings.h>

#include "table.h"
#include "util.h"

// Align with Opcode.
static const char *kOpTable[] = {
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

  "fadd.d", "fsub.d", "fmul.d", "fdiv.d",
  "fmv.d", "fneg.d",
  "fmv.x.d", "fmv.x.w",
  "feq.d", "flt.d", "fle.d",
  "feq.s", "flt.s", "fle.s",
  "fld", "flw", "fsd", "fsw",
  "fcvt.d.w", "fcvt.d.wu", "fcvt.d.l", "fcvt.d.lu",
  "fcvt.w.d", "fcvt.wu.d", "fcvt.l.d", "fcvt.lu.d",
  "fcvt.d.s", "fcvt.s.d",
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

static int find_match_index(const char **pp, const char **table, size_t count) {
  const char *p = *pp;
  const char *start = p;

  while (isalnum(*p) || *p == '.')
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

static enum FRegType find_fregister(const char **pp) {
  const char *p = *pp;
  for (int i = 0; i < (int)ARRAY_SIZE(kFRegisters); ++i) {
    const char *name = kFRegisters[i].name;
    size_t n = strlen(name);
    if (strncmp(p, name, n) == 0 && !is_label_chr(p[n])) {
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
    if (strncmp(p, name, n) == 0 && !is_label_chr(p[n])) {
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

static bool parse_operand(ParseInfo *info, Operand *operand, bool search_round_mode) {
  if (search_round_mode) {
    enum RoundMode roundmode = find_round_mode(&info->p);
    if (roundmode != NOROUND) {
      operand->type = ROUNDMODE;
      operand->roundmode = roundmode;
      return true;
    }
  }

  enum RegType reg = find_register(&info->p);
  if (reg != NOREG) {
    operand->type = REG;
    operand->reg.no = reg - X0;
    return true;
  }

  enum FRegType freg = find_fregister(&info->p);
  if (freg != NOFREG) {
    operand->type = FREG;
    operand->freg = freg;
    return true;
  }

  Expr *expr = parse_expr(info);
  if (*info->p == '(') {
    info->p += 1;
    return parse_indirect_register(info, expr, operand);
  }

  if (expr != NULL) {
    if (expr->kind == EX_FIXNUM) {
      operand->type = IMMEDIATE;
      operand->immediate = expr->fixnum;
      return true;
    }
    operand->type = DIRECT;
    operand->direct.expr = expr;
    return true;
  }

  return false;
}

static const Name *alloc_dummy_label(void) {
  // TODO: Ensure label is unique.
  static int label_no;
  ++label_no;
  char buf[2 + sizeof(int) * 3 + 1];
  snprintf(buf, sizeof(buf), "._%d", label_no);
  return alloc_name(buf, NULL, true);
}

void parse_inst(ParseInfo *info, Line *line) {
  Inst *inst  = &line->inst;
  Operand *opr_table[] = {&inst->opr1, &inst->opr2, &inst->opr3};
  for (int i = 0; i < (int)ARRAY_SIZE(opr_table); ++i)
    opr_table[i]->type = NOOPERAND;

  enum Opcode op = find_opcode(info);
  inst->op = op;
  if (op != NOOP) {
    for (int i = 0; i < (int)ARRAY_SIZE(opr_table); ++i) {
      if (!parse_operand(info, opr_table[i], i >= 2))
        break;
      info->p = skip_whitespaces(info->p);
      if (i == (int)ARRAY_SIZE(opr_table) - 1 || *info->p != ',')
        break;
      info->p = skip_whitespaces(info->p + 1);
    }
  }

  // Tweak for instruction.
  switch (inst->op) {
  case LA:
    // Store corresponding label to opr3.
    if (line->label == NULL) {
      // Generate unique label.
      const Name *label = alloc_dummy_label();
      line->label = label;
    }
    if (inst->opr3.type == NOOPERAND) {
      Expr *expr = new_expr(EX_LABEL);
      expr->label = line->label;

      Operand *opr = &inst->opr3;
      opr->type = DIRECT;
      opr->direct.expr = expr;
    }
    break;
  default: break;
  }
}
