#include "../../../config.h"
#include "parse_asm.h"

#include <assert.h>
#include <ctype.h>
#include <stddef.h>
#include <string.h>
#include <strings.h>

#include "util.h"

// Align with Opcode.
static const char *kOpTable[] = {
  "mov",
  "movb",  "movw",  "movl",  "movq",
  "movsx",  "movzx",
  "lea",

  "add",  "addq",
  "sub",  "subq",
  "mul",
  "div",  "idiv",
  "neg",
  "not",
  "inc",  "incb",  "incw",  "incl",  "incq",
  "dec",  "decb",  "decw",  "decl",  "decq",
  "and",
  "or",
  "xor",
  "shl",
  "shr",
  "sar",
  "cmp",
  "test",
  "cwtl",  "cltd",  "cqto",

  "seto",  "setno",  "setb",  "setae",  "sete",  "setne",  "setbe",  "seta",
  "sets",  "setns",  "setp",  "setnp",  "setl",  "setge",  "setle",  "setg",

  "jmp",
  "jo",  "jno",  "jb",  "jae",  "je",  "jne",  "jbe",  "ja",
  "js",  "jns",  "jp",  "jnp",  "jl",  "jge",  "jle",  "jg",
  "call",  "ret",
  "push",  "pop",

  "int",
  "syscall",

  "movsd",
  "addsd",
  "subsd",
  "mulsd",
  "divsd",
  "xorpd",
  "ucomisd",
  "cvtsi2sd",  "cvttsd2si",
  "sqrtsd",

  "movss",
  "addss",
  "subss",
  "mulss",
  "divss",
  "xorps",
  "ucomiss",
  "cvtsi2ss",  "cvttss2si",
  "cvtsd2ss",  "cvtss2sd",
};

static const struct {
  const char *name;
  enum RegType reg;
} kRegisters[] = {
  // 8bit
  {"al", AL},     {"cl", CL},     {"dl", DL},     {"bl", BL},
  {"ah", AH},     {"ch", CH},     {"dh", DH},     {"bh", BH},
  {"r8b", R8B},   {"r9b", R9B},   {"r10b", R10B}, {"r11b", R11B},
  {"r12b", R12B}, {"r13b", R13B}, {"r14b", R14B}, {"r15b", R15B},
  {"spl", SPL},   {"bpl", BPL},   {"sil", SIL},   {"dil", DIL},

  // 16bit
  {"ax", AX},     {"cx", CX},     {"dx", DX},     {"bx", BX},
  {"sp", SP},     {"bp", BP},     {"si", SI},     {"di", DI},
  {"r8w", R8W},   {"r9w", R9W},   {"r10w", R10W}, {"r11w", R11W},
  {"r12w", R12W}, {"r13w", R13W}, {"r14w", R14W}, {"r15w", R15W},

  // 32bit
  {"eax", EAX},   {"ecx", ECX},   {"edx", EDX},   {"ebx", EBX},
  {"esp", ESP},   {"ebp", EBP},   {"esi", ESI},   {"edi", EDI},
  {"r8d", R8D},   {"r9d", R9D},   {"r10d", R10D}, {"r11d", R11D},
  {"r12d", R12D}, {"r13d", R13D}, {"r14d", R14D}, {"r15d", R15D},

  // 64bit
  {"rax", RAX}, {"rcx", RCX}, {"rdx", RDX}, {"rbx", RBX},
  {"rsp", RSP}, {"rbp", RBP}, {"rsi", RSI}, {"rdi", RDI},
  {"r8", R8},   {"r9", R9},   {"r10", R10}, {"r11", R11},
  {"r12", R12}, {"r13", R13}, {"r14", R14}, {"r15", R15},
  {"rip", RIP},

  // Segment register
  {"cs", CS}, {"ds", DS}, {"es", ES}, {"fs", FS}, {"gs", GS}, {"ss", SS},
};

static const char kXmmRegisters[][6] = {
  "xmm0",  "xmm1",  "xmm2",  "xmm3",  "xmm4",  "xmm5",  "xmm6",  "xmm7",
  "xmm8",  "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",
};

inline bool is_reg8(enum RegType reg) {
  return reg >= AL && reg <= DIL;
}

inline bool is_reg16(enum RegType reg) {
  return reg >= AX && reg <= R15W;
}

inline bool is_reg32(enum RegType reg) {
  return reg >= EAX && reg <= R15D;
}

inline bool is_reg64(enum RegType reg) {
  return reg >= RAX && reg <= R15;
}

inline bool is_segment(enum RegType reg) {
  return reg >= CS && reg <= SS;
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

static enum RegXmmType find_xmm_register(const char **pp) {
  const char *p = *pp;
  const char *q;
  for (q = p; isalnum(*q); ++q)
    ;
  size_t l = q - p;

  for (int i = 0; i < (int)ARRAY_SIZE(kXmmRegisters); ++i) {
    const char *name = kXmmRegisters[i];
    size_t n = strlen(name);
    if (l == n && strncmp(p, name, n) == 0) {
      *pp = p + n;
      return i + XMM0;
    }
  }
  return NOREGXMM;
}

static enum RegType parse_direct_register(ParseInfo *info, Operand *operand) {
  {
    enum RegXmmType regxmm = find_xmm_register(&info->p);
    if (regxmm != NOREGXMM) {
      operand->type = REG_XMM;
      operand->regxmm = regxmm;
      return true;
    }
  }

  enum RegType reg = find_register(&info->p);
  if (is_segment(reg)) {
    Expr *offset = NULL;
    if (*info->p == ':') {
      ++info->p;
      offset = parse_expr(info);
    }
    operand->type = SEGMENT_OFFSET;
    operand->segment.reg = reg;
    operand->segment.offset = offset;
    return true;
  }

  enum RegSize size;
  int no;
  if (is_reg8(reg)) {
    size = REG8;
    no = reg - AL;
  } else if (is_reg16(reg)) {
    size = REG16;
    no = reg - AX;
  } else if (is_reg32(reg)) {
    size = REG32;
    no = reg - EAX;
  } else if (is_reg64(reg)) {
    size = REG64;
    no = reg - RAX;
  } else {
    parse_error(info, "Illegal register");
    return false;
  }

  operand->type = REG;
  operand->reg.size = size;
  operand->reg.no = no & 7;
  operand->reg.x = no >> 3;
  return true;
}

static bool parse_indirect_register(ParseInfo *info, Expr *offset, Operand *operand) {
  enum RegType index_reg = NOREG;
  Expr *scale = NULL;
  // Already read "(%".
  enum RegType base_reg = find_register(&info->p);

  info->p = skip_whitespaces(info->p);
  if (*info->p == ',') {
    info->p = skip_whitespaces(info->p + 1);
    if (*info->p != '%' ||
        (++info->p, index_reg = find_register(&info->p), !is_reg64(index_reg)))
      parse_error(info, "Register expected");
    info->p = skip_whitespaces(info->p);
    if (*info->p == ',') {
      info->p = skip_whitespaces(info->p + 1);
      scale = parse_expr(info);
      if (scale->kind != EX_FIXNUM)
        parse_error(info, "constant value expected");
      info->p = skip_whitespaces(info->p);
    }
  }
  if (*info->p != ')')
    parse_error(info, "`)' expected");
  else
    ++info->p;

  if (!(is_reg64(base_reg) || (base_reg == RIP && index_reg == NOREG)))
    parse_error(info, "Register expected");

  if (index_reg == NOREG) {
    char no = base_reg - RAX;
    operand->type = INDIRECT;
    operand->indirect.reg.size = REG64;
    operand->indirect.reg.no = base_reg != RIP ? no & 7 : RIP;
    operand->indirect.reg.x = (no & 8) >> 3;
    operand->indirect.offset = offset;
  } else {
    if (!is_reg64(index_reg))
      parse_error(info, "Register expected");

    operand->type = INDIRECT_WITH_INDEX;
    operand->indirect_with_index.offset = offset;
    operand->indirect_with_index.scale = scale;
    char base_no = base_reg - RAX;
    operand->indirect_with_index.base_reg.size = REG64;
    operand->indirect_with_index.base_reg.no = base_no & 7;
    operand->indirect_with_index.base_reg.x = (base_no & 8) >> 3;
    char index_no = index_reg - RAX;
    operand->indirect_with_index.index_reg.size = REG64;
    operand->indirect_with_index.index_reg.no = index_no & 7;
    operand->indirect_with_index.index_reg.x = (index_no & 8) >> 3;
  }

  return true;
}

static enum RegType parse_deref_register(ParseInfo *info, Operand *operand) {
  enum RegType reg = find_register(&info->p);
  if (!is_reg64(reg))
    parse_error(info, "Illegal register");

  char no = reg - RAX;
  operand->type = DEREF_REG;
  operand->reg.size = REG64;
  operand->reg.no = no & 7;
  operand->reg.x = (no & 8) >> 3;
  return true;
}

static bool parse_deref_indirect(ParseInfo *info, Operand *operand) {
  Expr *offset = parse_expr(info);
  info->p = skip_whitespaces(info->p);
  if (*info->p != '(') {
    parse_error(info, "direct number not implemented");
    return false;
  }
  if (info->p[1] != '%') {
    parse_error(info, "Register expected");
    return false;
  }
  info->p += 2;

  enum RegType index_reg = NOREG;
  Expr *scale = NULL;
  // Already read "(%".
  enum RegType base_reg = find_register(&info->p);

  info->p = skip_whitespaces(info->p);
  if (*info->p == ',') {
    info->p = skip_whitespaces(info->p + 1);
    if (*info->p != '%' ||
        (++info->p, index_reg = find_register(&info->p), !is_reg64(index_reg)))
      parse_error(info, "Register expected");
    info->p = skip_whitespaces(info->p);
    if (*info->p == ',') {
      info->p = skip_whitespaces(info->p + 1);
      scale = parse_expr(info);
      if (scale->kind != EX_FIXNUM)
        parse_error(info, "constant value expected");
      info->p = skip_whitespaces(info->p);
    }
  }
  if (*info->p != ')')
    parse_error(info, "`)' expected");
  else
    ++info->p;

  if (!is_reg64(base_reg) || (index_reg != NOREG && !is_reg64(index_reg)))
    parse_error(info, "Register expected");

  if (index_reg == NOREG) {
    operand->type = DEREF_INDIRECT;
    operand->indirect.offset = offset;
    char reg_no = base_reg - RAX;
    operand->indirect.reg.size = REG64;
    operand->indirect.reg.no = reg_no & 7;
    operand->indirect.reg.x = (reg_no & 8) >> 3;
  } else {
    operand->type = DEREF_INDIRECT_WITH_INDEX;
    operand->indirect_with_index.offset = offset;
    operand->indirect_with_index.scale = scale;
    char base_no = base_reg - RAX;
    operand->indirect_with_index.base_reg.size = REG64;
    operand->indirect_with_index.base_reg.no = base_no & 7;
    operand->indirect_with_index.base_reg.x = (base_no & 8) >> 3;
    operand->indirect_with_index.index_reg.size = REG64;
    char index_no = index_reg - RAX;
    operand->indirect_with_index.index_reg.size = REG64;
    operand->indirect_with_index.index_reg.no = index_no & 7;
    operand->indirect_with_index.index_reg.x = (index_no & 8) >> 3;
  }

  return true;
}

static bool parse_operand(ParseInfo *info, Operand *operand) {
  const char *p = info->p;
  if (*p == '%') {
    info->p = p + 1;
    return parse_direct_register(info, operand);
  }

  if (*p == '*') {
    if (p[1] == '%') {
      info->p = p + 2;
      return parse_deref_register(info, operand);
    } else {
      info->p = p + 1;
      return parse_deref_indirect(info, operand);
    }
  }

  if (*p == '$') {
    info->p = p + 1;
    if (!immediate(&info->p, &operand->immediate))
      parse_error(info, "Syntax error");
    operand->type = IMMEDIATE;
    return true;
  }

  Expr *expr = parse_expr(info);
  info->p = skip_whitespaces(info->p);
  if (*info->p != '(') {
    if (expr != NULL) {
      if (expr->kind == EX_LABEL || expr->kind == EX_FIXNUM) {
        operand->type = DIRECT;
        operand->direct.expr = expr;
        return true;
      }
      parse_error(info, "direct number not implemented");
    }
  } else {
    if (info->p[1] == '%') {
      info->p += 2;
      if (expr == NULL) {
        expr = malloc_or_die(sizeof(*expr));
        expr->kind = EX_FIXNUM;
        expr->fixnum = 0;
      }
      return parse_indirect_register(info, expr, operand);
    }
  }

  return false;
}

void parse_inst(ParseInfo *info, Inst *inst) {
  Operand *opr_table[] = {&inst->src, &inst->dst};
  for (int i = 0; i < (int)ARRAY_SIZE(opr_table); ++i)
    opr_table[i]->type = NOOPERAND;

  enum Opcode op = find_opcode(info);
  inst->op = op;
  if (op != NOOP) {
    if (parse_operand(info, &inst->src)) {
      info->p = skip_whitespaces(info->p);
      if (*info->p == ',') {
        info->p = skip_whitespaces(info->p + 1);
        parse_operand(info, &inst->dst);
        info->p = skip_whitespaces(info->p);
      }
    }
  }
}
