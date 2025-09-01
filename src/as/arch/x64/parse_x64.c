#include "../../../config.h"
#include "parse_asm.h"

#include <assert.h>
#include <ctype.h>
#include <stddef.h>
#include <string.h>
#include <strings.h>

#include "inst.h"
#include "util.h"

enum RawOpcode {
  R_NOOP,
  R_MOV,
  R_MOVB, R_MOVW, R_MOVL, R_MOVQ,
  R_MOVSX, R_MOVZX,
  R_LEA,

  R_ADD, R_ADDQ,
  R_SUB, R_SUBQ,
  R_MUL,
  R_DIV, R_IDIV,
  R_NEG,
  R_NOT,
  R_INC, R_INCB, R_INCW, R_INCL, R_INCQ,
  R_DEC, R_DECB, R_DECW, R_DECL, R_DECQ,
  R_AND, R_OR, R_XOR,
  R_SHL, R_SHR, R_SAR,
  R_CMP,
  R_TEST,
  R_CWTL, R_CLTD, R_CQTO,
  R_BSR, R_LZCNT, R_TZCNT, R_POPCNT,

  R_SETO, R_SETNO, R_SETB, R_SETAE, R_SETE, R_SETNE, R_SETBE, R_SETA,
  R_SETS, R_SETNS, R_SETP, R_SETNP, R_SETL, R_SETGE, R_SETLE, R_SETG,

  R_JMP,
  R_JO, R_JNO, R_JB, R_JAE, R_JE, R_JNE, R_JBE, R_JA,
  R_JS, R_JNS, R_JP, R_JNP, R_JL, R_JGE, R_JLE, R_JG,
  R_CALL, R_RET,
  R_PUSH, R_POP,

  R_INT, R_SYSCALL,

  R_MOVSD, R_ADDSD, R_SUBSD, R_MULSD, R_DIVSD, R_XORPD,
  R_COMISD, R_UCOMISD,
  R_CVTSI2SD, R_CVTTSD2SI,
  R_SQRTSD,

  R_MOVSS, R_ADDSS, R_SUBSS, R_MULSS, R_DIVSS, R_XORPS,
  R_COMISS, R_UCOMISS,
  R_CVTSI2SS, R_CVTTSS2SI,
  R_CVTSD2SS, R_CVTSS2SD,

  R_ENDBR64,
};

const char *kRawOpTable[] = {
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
  "and", "or", "xor",
  "shl", "shr", "sar",
  "cmp",
  "test",
  "cwtl",  "cltd",  "cqto",
  "bsr", "lzcnt", "tzcnt", "popcnt",

  "seto",  "setno",  "setb",  "setae",  "sete",  "setne",  "setbe",  "seta",
  "sets",  "setns",  "setp",  "setnp",  "setl",  "setge",  "setle",  "setg",

  "jmp",
  "jo",  "jno",  "jb",  "jae",  "je",  "jne",  "jbe",  "ja",
  "js",  "jns",  "jp",  "jnp",  "jl",  "jge",  "jle",  "jg",
  "call",  "ret",
  "push",  "pop",

  "int", "syscall",

  "movsd", "addsd", "subsd", "mulsd", "divsd", "xorpd",
  "comisd", "ucomisd",
  "cvtsi2sd",  "cvttsd2si",
  "sqrtsd",

  "movss", "addss", "subss", "mulss", "divss", "xorps",
  "comiss", "ucomiss",
  "cvtsi2ss",  "cvttss2si",
  "cvtsd2ss",  "cvtss2sd",

  "endbr64",
  NULL,
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

static inline bool is_reg8(enum RegType reg) {
  return reg >= AL && reg <= DIL;
}

static inline bool is_reg16(enum RegType reg) {
  return reg >= AX && reg <= R15W;
}

static inline bool is_reg32(enum RegType reg) {
  return reg >= EAX && reg <= R15D;
}

static inline bool is_reg64(enum RegType reg) {
  return reg >= RAX && reg <= R15;
}

static inline bool is_segment(enum RegType reg) {
  return reg >= CS && reg <= SS;
}

#define R8    (1 << 0)
#define R16   (1 << 1)
#define R32   (1 << 2)
#define R64   (1 << 3)
#define R8CL  (1 << 4)
#define IMM   (1 << 5)
#define EXP   (1 << 6)
#define IND   (1 << 7)
#define IIND  (1 << 8)
#define DER   (1 << 9)   // Deref Reg64
#define DEI   (1 << 10)  // Deref Indirect
#define DEII  (1 << 11)  // Deref Indirect with Index
#define XMM   (1 << 12)
#define SEG   (1 << 13)

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

static enum RegXmmType find_xmm_register(const char **pp) {
  const char *p = *pp;
  const char *q;
  for (q = p; isalnum(*q); ++q)
    ;
  size_t l = q - p;

  for (int i = 0; i < (int)ARRAY_SIZE(kXmmRegisters); ++i) {
    const char *name = kXmmRegisters[i];
    size_t n = strlen(name);
    if (l == n && strncasecmp(p, name, n) == 0) {
      *pp = p + n;
      return i + XMM0;
    }
  }
  return NOREGXMM;
}

static unsigned int parse_direct_register(ParseInfo *info, Operand *operand) {
  {
    enum RegXmmType regxmm = find_xmm_register(&info->p);
    if (regxmm != NOREGXMM) {
      operand->type = REG_XMM;
      operand->regxmm = regxmm;
      return XMM;
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
    return SEG;
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
  return (R8 << (size - REG8)) | (reg == CL ? R8CL : 0);
}

#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
static int parse_label_postfix(ParseInfo *info) {
  static struct {
    const char *name;
    int flag;
  } const kPostfixes[] = {
    {"@gotpcrel", LF_GOTPCREL},
  };
  const char *p = info->p;
  for (size_t i = 0; i < ARRAY_SIZE(kPostfixes); ++i) {
    const char *name = kPostfixes[i].name;
    size_t n = strlen(name);
    if (strncasecmp(p, name, n) == 0 && !is_label_chr(p[n])) {
      info->p = p + n;
      return kPostfixes[i].flag;
    }
  }
  return 0;
}
#endif

static ExprWithFlag parse_expr_with_flag(ParseInfo *info) {
  // expr = label + nn
#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
  // expr@page
  // expr@pageoff
  // expr@gotpage
  // expr@gotpageoff
  Expr *expr = parse_expr(info);
  int flag = parse_label_postfix(info);
#else
  int flag = 0;
  Expr *expr = parse_expr(info);
#endif
  return (ExprWithFlag){expr, flag};
}

static unsigned int parse_indirect_register(ParseInfo *info, ExprWithFlag *offset, Operand *operand) {
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
    operand->indirect.offset = *offset;
    return IND;
  } else {
    if (!is_reg64(index_reg))
      parse_error(info, "Register expected");

    operand->type = INDIRECT_WITH_INDEX;
    operand->indirect_with_index.offset = offset->expr;
    operand->indirect_with_index.scale = scale;
    char base_no = base_reg - RAX;
    operand->indirect_with_index.base_reg.size = REG64;
    operand->indirect_with_index.base_reg.no = base_no & 7;
    operand->indirect_with_index.base_reg.x = (base_no & 8) >> 3;
    char index_no = index_reg - RAX;
    operand->indirect_with_index.index_reg.size = REG64;
    operand->indirect_with_index.index_reg.no = index_no & 7;
    operand->indirect_with_index.index_reg.x = (index_no & 8) >> 3;
    return IIND;
  }
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

static unsigned int parse_deref_indirect(ParseInfo *info, Operand *operand) {
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
    operand->indirect.offset.expr = offset;
    operand->indirect.offset.flag = 0;
    char reg_no = base_reg - RAX;
    operand->indirect.reg.size = REG64;
    operand->indirect.reg.no = reg_no & 7;
    operand->indirect.reg.x = (reg_no & 8) >> 3;
    return DEI;
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
    return DEII;
  }
}

unsigned int parse_operand(ParseInfo *info, unsigned int opr_flag, Operand *operand) {
  const char *p = info->p;
  if (opr_flag & (R8 | R16 | R32 | R64 | R8CL | XMM)) {
    if (*p == '%') {
      info->p = p + 1;
      return parse_direct_register(info, operand) & opr_flag;
    }
  }

  if (opr_flag & (DER | DEI)) {
    if (*p == '*') {
      if (opr_flag & DER) {
        if (p[1] == '%') {
          info->p = p + 2;
          if (parse_deref_register(info, operand))
            return DER;
        }
      }
      if (opr_flag & DEI) {
        info->p = p + 1;
        return parse_deref_indirect(info, operand);
      }
    }
  }

  if (opr_flag & IMM) {
    if (*p == '$') {
      info->p = p + 1;
      if (!immediate(&info->p, &operand->immediate))
        parse_error(info, "Syntax error");
      operand->type = IMMEDIATE;
      return IMM;
    }
  }

  ExprWithFlag expr_with_flag = parse_expr_with_flag(info);
  info->p = skip_whitespaces(info->p);
  if (*info->p != '(') {
    if (expr_with_flag.expr != NULL) {
      if (expr_with_flag.expr->kind == EX_LABEL || expr_with_flag.expr->kind == EX_FIXNUM) {
        operand->type = DIRECT;
        operand->direct.expr = expr_with_flag.expr;
        return EXP;
      }
      parse_error(info, "direct number not implemented");
    }
  } else {
    if (info->p[1] == '%') {
      info->p += 2;
      if (expr_with_flag.expr == NULL) {
        Expr *expr = calloc_or_die(sizeof(*expr));
        expr->kind = EX_FIXNUM;
        expr->fixnum = 0;
        expr_with_flag.expr = expr;
      }
      return parse_indirect_register(info, &expr_with_flag, operand);
    }
  }

  return 0;
}

const ParseInstTable kParseInstTable[] = {
  [R_MOV] = { 11, (const ParseOpArray*[]){
    &(ParseOpArray){MOV_RR, {R8, R8}},     &(ParseOpArray){MOV_RR, {R16, R16}},
    &(ParseOpArray){MOV_RR, {R32, R32}},   &(ParseOpArray){MOV_RR, {R64, R64}},
    &(ParseOpArray){MOV_IMR, {IMM, R8 | R16 | R32 | R64}},
    &(ParseOpArray){MOV_IR, {IND, R8 | R16 | R32 | R64}},
    &(ParseOpArray){MOV_RI, {R8 | R16 | R32 | R64, IND}},
    &(ParseOpArray){MOV_IIR, {IIND, R8 | R16 | R32 | R64}},
    &(ParseOpArray){MOV_DR, {EXP, R8 | R16 | R32 | R64}},
    &(ParseOpArray){MOV_RD, {R8 | R16 | R32 | R64, EXP}},
    &(ParseOpArray){MOV_SR, {SEG, R64}},
  } },
  [R_MOVB] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){MOVB_IMI, {IMM, IND}},
    &(ParseOpArray){MOVB_IMD, {IMM, EXP}},
  } },
  [R_MOVW] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){MOVW_IMI, {IMM, IND}},
    &(ParseOpArray){MOVW_IMD, {IMM, EXP}},
  } },
  [R_MOVL] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){MOVL_IMI, {IMM, IND}},
    &(ParseOpArray){MOVL_IMD, {IMM, EXP}},
  } },
  [R_MOVQ] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){MOVQ_IMI, {IMM, IND}},
    &(ParseOpArray){MOVQ_IMD, {IMM, EXP}},
  } },
  [R_MOVSX] = { 6, (const ParseOpArray*[]){
    &(ParseOpArray){MOVSX, {R8, R16}},
    &(ParseOpArray){MOVSX, {R8, R32}},
    &(ParseOpArray){MOVSX, {R8, R64}},
    &(ParseOpArray){MOVSX, {R16, R32}},
    &(ParseOpArray){MOVSX, {R16, R64}},
    &(ParseOpArray){MOVSX, {R32, R64}},
  } },
  [R_MOVZX] = { 6, (const ParseOpArray*[]){
    &(ParseOpArray){MOVZX, {R8, R16}},
    &(ParseOpArray){MOVZX, {R8, R32}},
    &(ParseOpArray){MOVZX, {R8, R64}},
    &(ParseOpArray){MOVZX, {R16, R32}},
    &(ParseOpArray){MOVZX, {R16, R64}},
    &(ParseOpArray){MOVZX, {R32, R64}},
  } },
  [R_LEA] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){LEA_IR, {IND, R64}},
    &(ParseOpArray){LEA_IIR, {IIND, R64}},
  } },
  [R_ADD] = { 7, (const ParseOpArray*[]){
    &(ParseOpArray){ADD_RR, {R8, R8}},     &(ParseOpArray){ADD_RR, {R16, R16}},
    &(ParseOpArray){ADD_RR, {R32, R32}},   &(ParseOpArray){ADD_RR, {R64, R64}},
    &(ParseOpArray){ADD_IMR, {IMM, R8 | R16 | R32 | R64}},
    &(ParseOpArray){ADD_IR, {IND, R8 | R16 | R32 | R64}},
    &(ParseOpArray){ADD_IIR, {IIND, R8 | R16 | R32 | R64}},
  } },
  [R_ADDQ] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){ADDQ, {IMM, IND}} } },
  [R_SUB] = { 7, (const ParseOpArray*[]){
    &(ParseOpArray){SUB_RR, {R8, R8}},     &(ParseOpArray){SUB_RR, {R16, R16}},
    &(ParseOpArray){SUB_RR, {R32, R32}},   &(ParseOpArray){SUB_RR, {R64, R64}},
    &(ParseOpArray){SUB_IMR, {IMM, R8 | R16 | R32 | R64}},
    &(ParseOpArray){SUB_IR, {IND, R8 | R16 | R32 | R64}},
    &(ParseOpArray){SUB_IIR, {IIND, R8 | R16 | R32 | R64}},
  } },
  [R_SUBQ] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SUBQ, {IMM, IND}} } },
  [R_MUL] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){MUL, {R8 | R16 | R32 | R64}} } },
  [R_DIV] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){DIV, {R8 | R16 | R32 | R64}} } },
  [R_IDIV] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){IDIV, {R8 | R16 | R32 | R64}} } },
  [R_NEG] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){NEG, {R8 | R16 | R32 | R64}} } },
  [R_NOT] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){NOT, {R8 | R16 | R32 | R64}} } },
  [R_INC] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){INC, {R8 | R16 | R32 | R64}} } },
  [R_INCB] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){INCB, {IND}} } },
  [R_INCW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){INCW, {IND}} } },
  [R_INCL] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){INCL, {IND}} } },
  [R_INCQ] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){INCQ, {IND}} } },
  [R_DEC] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){DEC, {R8 | R16 | R32 | R64}} } },
  [R_DECB] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){DECB, {IND}} } },
  [R_DECW] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){DECW, {IND}} } },
  [R_DECL] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){DECL, {IND}} } },
  [R_DECQ] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){DECQ, {IND}} } },
  [R_AND] = { 5, (const ParseOpArray*[]){
    &(ParseOpArray){AND_RR, {R8, R8}},     &(ParseOpArray){AND_RR, {R16, R16}},
    &(ParseOpArray){AND_RR, {R32, R32}},   &(ParseOpArray){AND_RR, {R64, R64}},
    &(ParseOpArray){AND_IMR, {IMM, R8 | R16 | R32 | R64}},
  } },
  [R_OR] = { 5, (const ParseOpArray*[]){
    &(ParseOpArray){OR_RR, {R8, R8}},     &(ParseOpArray){OR_RR, {R16, R16}},
    &(ParseOpArray){OR_RR, {R32, R32}},   &(ParseOpArray){OR_RR, {R64, R64}},
    &(ParseOpArray){OR_IMR, {IMM, R8 | R16 | R32 | R64}},
  } },
  [R_XOR] = { 5, (const ParseOpArray*[]){
    &(ParseOpArray){XOR_RR, {R8, R8}},     &(ParseOpArray){XOR_RR, {R16, R16}},
    &(ParseOpArray){XOR_RR, {R32, R32}},   &(ParseOpArray){XOR_RR, {R64, R64}},
    &(ParseOpArray){XOR_IMR, {IMM, R8 | R16 | R32 | R64}},
  } },
  [R_SHL] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){SHL_RR, {R8CL, R8 | R16 | R32 | R64}},  // %cl, %reg
    &(ParseOpArray){SHL_IMR, {IMM, R8 | R16 | R32 | R64}},
  } },
  [R_SHR] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){SHR_RR, {R8CL, R8 | R16 | R32 | R64}},  // %cl, %reg
    &(ParseOpArray){SHR_IMR, {IMM, R8 | R16 | R32 | R64}},
  } },
  [R_SAR] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){SAR_RR, {R8CL, R8 | R16 | R32 | R64}},  // %cl, %reg
    &(ParseOpArray){SAR_IMR, {IMM, R8 | R16 | R32 | R64}},
  } },
  [R_CMP] = { 5, (const ParseOpArray*[]){
    &(ParseOpArray){CMP_RR, {R8, R8}},     &(ParseOpArray){CMP_RR, {R16, R16}},
    &(ParseOpArray){CMP_RR, {R32, R32}},   &(ParseOpArray){CMP_RR, {R64, R64}},
    &(ParseOpArray){CMP_IMR, {IMM, R8 | R16 | R32 | R64}},
  } },
  [R_TEST] = { 4, (const ParseOpArray*[]){
    &(ParseOpArray){TEST, {R8, R8}},     &(ParseOpArray){TEST, {R16, R16}},
    &(ParseOpArray){TEST, {R32, R32}},   &(ParseOpArray){TEST, {R64, R64}},
  } },
  [R_CWTL] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){CWTL} } },
  [R_CLTD] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){CLTD} } },
  [R_CQTO] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){CQTO} } },
  [R_BSR] = { 3, (const ParseOpArray*[]){
    &(ParseOpArray){BSR, {R16, R16}},   &(ParseOpArray){BSR, {R32, R32}},
    &(ParseOpArray){BSR, {R64, R64}},
  } },
  [R_LZCNT] = { 3, (const ParseOpArray*[]){
    &(ParseOpArray){LZCNT, {R16, R16}},   &(ParseOpArray){LZCNT, {R32, R32}},
    &(ParseOpArray){LZCNT, {R64, R64}},
  } },
  [R_TZCNT] = { 3, (const ParseOpArray*[]){
    &(ParseOpArray){TZCNT, {R16, R16}},   &(ParseOpArray){TZCNT, {R32, R32}},
    &(ParseOpArray){TZCNT, {R64, R64}},
  } },
  [R_POPCNT] = { 3, (const ParseOpArray*[]){
    &(ParseOpArray){POPCNT, {R16, R16}},   &(ParseOpArray){POPCNT, {R32, R32}},
    &(ParseOpArray){POPCNT, {R64, R64}},
  } },
  [R_SETO]  = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SETO,  {R8}} } },
  [R_SETNO] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SETNO, {R8}} } },
  [R_SETB]  = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SETB,  {R8}} } },
  [R_SETAE] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SETAE, {R8}} } },
  [R_SETE]  = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SETE,  {R8}} } },
  [R_SETNE] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SETNE, {R8}} } },
  [R_SETBE] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SETBE, {R8}} } },
  [R_SETA]  = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SETA,  {R8}} } },
  [R_SETS]  = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SETS,  {R8}} } },
  [R_SETNS] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SETNS, {R8}} } },
  [R_SETP]  = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SETP,  {R8}} } },
  [R_SETNP] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SETNP, {R8}} } },
  [R_SETL]  = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SETL,  {R8}} } },
  [R_SETGE] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SETGE, {R8}} } },
  [R_SETLE] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SETLE, {R8}} } },
  [R_SETG]  = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SETG,  {R8}} } },
  [R_JMP] = { 4, (const ParseOpArray*[]){
    &(ParseOpArray){JMP_D, {EXP}},
    &(ParseOpArray){JMP_DER, {DER}},
    &(ParseOpArray){JMP_DEI, {DEI}},
    &(ParseOpArray){JMP_DEII, {DEII}},
  } },
  [R_JO]  = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JO,  {EXP}} } },
  [R_JNO] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JNO, {EXP}} } },
  [R_JB]  = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JB,  {EXP}} } },
  [R_JAE] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JAE, {EXP}} } },
  [R_JE]  = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JE,  {EXP}} } },
  [R_JNE] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JNE, {EXP}} } },
  [R_JBE] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JBE, {EXP}} } },
  [R_JA]  = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JA,  {EXP}} } },
  [R_JS]  = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JS,  {EXP}} } },
  [R_JNS] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JNS, {EXP}} } },
  [R_JP]  = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JP,  {EXP}} } },
  [R_JNP] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JNP, {EXP}} } },
  [R_JL]  = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JL,  {EXP}} } },
  [R_JGE] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JGE, {EXP}} } },
  [R_JLE] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JLE, {EXP}} } },
  [R_JG]  = { 1, (const ParseOpArray*[]){ &(ParseOpArray){JG,  {EXP}} } },
  [R_CALL] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){CALL_D, {EXP}},
    &(ParseOpArray){CALL_DER, {DER}},
  } },
  [R_RET] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){RET} } },
  [R_PUSH] = { 2, (const ParseOpArray*[]){
    &(ParseOpArray){PUSH_R, {R64}},
    &(ParseOpArray){PUSH_IM, {IMM}},
  } },
  [R_POP] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){POP, {R64}} } },
  [R_INT] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){INT, {IMM}} } },
  [R_SYSCALL] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SYSCALL} } },

  [R_MOVSD] = { 3, (const ParseOpArray*[]){
    &(ParseOpArray){MOVSD_XX, {XMM, XMM}},
    &(ParseOpArray){MOVSD_IX, {IND, XMM}},
    &(ParseOpArray){MOVSD_XI, {XMM, IND}},
  } },
  [R_MOVSS] = { 3, (const ParseOpArray*[]){
    &(ParseOpArray){MOVSS_XX, {XMM, XMM}},
    &(ParseOpArray){MOVSS_IX, {IND, XMM}},
    &(ParseOpArray){MOVSS_XI, {XMM, IND}},
  } },
  [R_ADDSD] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){ADDSD, {XMM, XMM}}, } },
  [R_ADDSS] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){ADDSS, {XMM, XMM}}, } },
  [R_SUBSD] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SUBSD, {XMM, XMM}}, } },
  [R_SUBSS] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SUBSS, {XMM, XMM}}, } },
  [R_MULSD] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){MULSD, {XMM, XMM}}, } },
  [R_MULSS] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){MULSS, {XMM, XMM}}, } },
  [R_DIVSD] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){DIVSD, {XMM, XMM}}, } },
  [R_DIVSS] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){DIVSS, {XMM, XMM}}, } },
  [R_XORPD] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){XORPD, {XMM, XMM}}, } },
  [R_XORPS] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){XORPS, {XMM, XMM}}, } },
  [R_COMISD] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){COMISD, {XMM, XMM}}, } },
  [R_UCOMISD] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){UCOMISD, {XMM, XMM}}, } },
  [R_COMISS] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){COMISS, {XMM, XMM}}, } },
  [R_UCOMISS] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){UCOMISS, {XMM, XMM}}, } },
  [R_CVTSI2SD] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){CVTSI2SD, {R32 | R64, XMM}}, } },
  [R_CVTSI2SS] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){CVTSI2SS, {R32 | R64, XMM}}, } },
  [R_CVTTSD2SI] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){CVTTSD2SI, {XMM, R32 | R64}}, } },
  [R_CVTTSS2SI] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){CVTTSS2SI, {XMM, R32 | R64}}, } },
  [R_CVTSD2SS] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){CVTSD2SS, {XMM, XMM}}, } },
  [R_CVTSS2SD] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){CVTSS2SD, {XMM, XMM}}, } },
  [R_SQRTSD] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){SQRTSD, {XMM, XMM}}, } },

  [R_ENDBR64] = { 1, (const ParseOpArray*[]){ &(ParseOpArray){ENDBR64}, } },
};
