#include "../config.h"
#include "parse_asm.h"

#include <assert.h>
#include <ctype.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>  // strtoul
#include <string.h>
#include <strings.h>

#include "gen_section.h"
#include "ir_asm.h"
#include "table.h"
#include "util.h"

static Expr *parse_expr(ParseInfo *info);

// Align with Opcode.
static const char *kOpTable[] = {
  "mov",
  "movb",
  "movw",
  "movl",
  "movq",
  "movsx",
  "movzx",
  "lea",

  "add",
  "addq",
  "sub",
  "subq",
  "mul",
  "div",
  "idiv",
  "neg",
  "not",
  "inc",
  "incb",
  "incw",
  "incl",
  "incq",
  "dec",
  "decb",
  "decw",
  "decl",
  "decq",
  "and",
  "or",
  "xor",
  "shl",
  "shr",
  "sar",
  "cmp",
  "test",
  "cwtl",
  "cltd",
  "cqto",

  "seto",
  "setno",
  "setb",
  "setae",
  "sete",
  "setne",
  "setbe",
  "seta",
  "sets",
  "setns",
  "setp",
  "setnp",
  "setl",
  "setge",
  "setle",
  "setg",

  "jmp",
  "jo",
  "jno",
  "jb",
  "jae",
  "je",
  "jne",
  "jbe",
  "ja",
  "js",
  "jns",
  "jp",
  "jnp",
  "jl",
  "jge",
  "jle",
  "jg",
  "call",
  "ret",
  "push",
  "pop",

  "int",
  "syscall",

#ifndef __NO_FLONUM
  "movsd",
  "addsd",
  "subsd",
  "mulsd",
  "divsd",
  "ucomisd",
  "cvtsi2sd",
  "cvttsd2si",
  "sqrtsd",

  "movss",
  "addss",
  "subss",
  "mulss",
  "divss",
  "ucomiss",
  "cvtsi2ss",
  "cvttss2si",

  "cvtsd2ss",
  "cvtss2sd",
#endif
};

static const struct {
  const char *name;
  enum RegType reg;
} kRegisters[] = {
  {"al", AL},
  {"cl", CL},
  {"dl", DL},
  {"bl", BL},
  {"ah", AH},
  {"ch", CH},
  {"dh", DH},
  {"bh", BH},

  {"r8b", R8B},
  {"r9b", R9B},
  {"r10b", R10B},
  {"r11b", R11B},
  {"r12b", R12B},
  {"r13b", R13B},
  {"r14b", R14B},
  {"r15b", R15B},

  {"spl", SPL},
  {"bpl", BPL},
  {"sil", SIL},
  {"dil", DIL},

  {"ax", AX},
  {"cx", CX},
  {"dx", DX},
  {"bx", BX},
  {"sp", SP},
  {"bp", BP},
  {"si", SI},
  {"di", DI},

  {"r8w", R8W},
  {"r9w", R9W},
  {"r10w", R10W},
  {"r11w", R11W},
  {"r12w", R12W},
  {"r13w", R13W},
  {"r14w", R14W},
  {"r15w", R15W},

  {"eax", EAX},
  {"ecx", ECX},
  {"edx", EDX},
  {"ebx", EBX},
  {"esp", ESP},
  {"ebp", EBP},
  {"esi", ESI},
  {"edi", EDI},

  {"r8d", R8D},
  {"r9d", R9D},
  {"r10d", R10D},
  {"r11d", R11D},
  {"r12d", R12D},
  {"r13d", R13D},
  {"r14d", R14D},
  {"r15d", R15D},

  {"rax", RAX},
  {"rcx", RCX},
  {"rdx", RDX},
  {"rbx", RBX},
  {"rsp", RSP},
  {"rbp", RBP},
  {"rsi", RSI},
  {"rdi", RDI},

  {"r8", R8},
  {"r9", R9},
  {"r10", R10},
  {"r11", R11},
  {"r12", R12},
  {"r13", R13},
  {"r14", R14},
  {"r15", R15},

  {"rip", RIP},
};

#ifndef __NO_FLONUM
static const char kXmmRegisters[][6] = {
  "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
  "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",
};
#endif

static const char *kDirectiveTable[] = {
  "ascii",
  "section",
  "text",
  "data",
  "align",
  "p2align",
  "byte",
  "word",
  "long",
  "quad",
  "comm",
  "globl",
  "local",
  "extern",
#ifndef __NO_FLONUM
  "float",
  "double",
#endif
};

bool err;

void parse_error(const ParseInfo *info, const char *message) {
  fprintf(stderr, "%s(%d): %s\n", info->filename, info->lineno, message);
  fprintf(stderr, "%s\n", info->rawline);
  err = true;
}

static bool is_reg8(enum RegType reg) {
  return reg >= AL && reg <= DIL;
}

static bool is_reg16(enum RegType reg) {
  return reg >= AX && reg <= R15W;
}

static bool is_reg32(enum RegType reg) {
  return reg >= EAX && reg <= R15D;
}

static bool is_reg64(enum RegType reg) {
  return reg >= RAX && reg <= R15;
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
  return find_match_index(&info->p, kOpTable, sizeof(kOpTable) / sizeof(*kOpTable)) + 1;
}

static enum DirectiveType find_directive(const char *p, size_t n) {
  const char **table = kDirectiveTable;
  size_t count = sizeof(kDirectiveTable) / sizeof(*kDirectiveTable);
  for (size_t i = 0; i < count; ++i) {
    const char *name = table[i];
    if (strncasecmp(p, name, n) == 0 && name[n] == '\0') {
      return i + 1;
    }
  }
  return -1;
}

static enum RegType find_register(const char **pp) {
  const char *p = *pp;
  if (*p == '%')
    ++p;  // Allow "%%eax", etc.
  for (int i = 0, len = sizeof(kRegisters) / sizeof(*kRegisters); i < len; ++i) {
    const char *name = kRegisters[i].name;
    size_t n = strlen(name);
    if (strncmp(p, name, n) == 0) {
      *pp = p + n;
      return kRegisters[i].reg;
    }
  }
  return NOREG;
}

#ifndef __NO_FLONUM
static enum RegXmmType find_xmm_register(const char **pp) {
  const char *p = *pp;
  const char *q;
  for (q = p; isalnum(*q); ++q)
    ;
  size_t l = q - p;

  for (int i = 0, len = sizeof(kXmmRegisters) / sizeof(*kXmmRegisters); i < len; ++i) {
    const char *name = kXmmRegisters[i];
    size_t n = strlen(name);
    if (l == n && strncmp(p, name, n) == 0) {
      *pp = p + n;
      return i + XMM0;
    }
  }
  return NOREGXMM;
}
#endif

static bool immediate(const char **pp, long *value) {
  const char *p = *pp;
  bool negative = false;
  if (*p == '-') {
    negative = true;
    ++p;
  }

  int base = 10;
  if (*p == '0') {
    char c = tolower(p[1]);
    if (c == 'x') {
      base = 16;
      p += 2;
      c = tolower(*p);
      if (!isxdigit(c))
        return false;
    } else if (isdigit(c)) {
      if (c >= '8')
        return false;
      base = 8;
    }
  }
  const char *q = p;
  unsigned long val = strtoul(p, (char**)&p, base);
  if (p == q)
    return false;

  *value = negative ? -val : val;
  *pp = p;
  return true;
}

static bool is_label_first_chr(char c) {
  return isalpha(c) || c == '_' || c == '.';
}

static bool is_label_chr(char c) {
  return is_label_first_chr(c) || isdigit(c);
}

static const char *skip_until_delimiter(const char *p) {
  if (*p == '"') {
    ++p;
    for (char c; c = *p, c != '\0'; ++p) {
      if (c == '"') {
        ++p;
        break;
      }
      if (c == '\\')
        ++p;
    }
  } else {
    for (char c; c = *p, !isspace(c) && c != ':' && c != '\0'; ++p)
      ;
  }
  return p;
}

static const Name *unquote_label(const char *p, const char *q) {
  if (*p != '"')
    return alloc_name(p, q, false);
  if (q[-1] != '"' || q == p + 2)
    return NULL;
  // TODO: Unquote
  return alloc_name(p + 1, q - 1, false);
}

static const Name *parse_label(ParseInfo *info) {
  const char *p = info->p;
  const char *start = p;
  if (*p == '"') {
    p = skip_until_delimiter(p);
    if (p[-1] != '"' || p == start + 2)
      return NULL;

    info->p = p;
    return alloc_name(start + 1, p - 1, false);
  }

  if (!is_label_first_chr(*p))
    return NULL;

  do {
    ++p;
  } while (is_label_chr(*p));
  info->p = p;
  return alloc_name(start, p, false);
}

static const Name *parse_section_name(ParseInfo *info) {
  const char *p = info->p;
  const char *start = p;

  if (!is_label_first_chr(*p))
    return NULL;

  do {
    ++p;
  } while (isalnum_(*p) || *p == '.');
  info->p = p;
  return alloc_name(start, p, false);
}

static enum RegType parse_direct_register(ParseInfo *info, Operand *operand) {
#ifndef __NO_FLONUM
  {
    enum RegXmmType regxmm = find_xmm_register(&info->p);
    if (regxmm != NOREGXMM) {
      operand->type = REG_XMM;
      operand->regxmm = regxmm;
      return true;
    }
  }
#endif

  enum RegType reg = find_register(&info->p);
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

enum TokenKind {
  TK_UNKNOWN,
  TK_LABEL,
  TK_FIXNUM,
  TK_ADD,
  TK_SUB,
  TK_MUL,
  TK_DIV,
#ifndef __NO_FLONUM
  TK_FLONUM,
#endif
};

typedef struct Token {
  enum TokenKind kind;
  union {
    const Name *label;
    int64_t fixnum;
#ifndef __NO_FLONUM
    double flonum;
#endif
  };
} Token;

static Token *new_token(enum TokenKind kind) {
  Token *token = malloc(sizeof(*token));
  token->kind = kind;
  return token;
}

#ifndef __NO_FLONUM
static const Token *read_flonum(ParseInfo *info) {
  const char *p = info->p;
  char *q;
  double f = strtod(p, &q);
  Token *token = new_token(TK_FLONUM);
  token->flonum = f;
  info->next = q;
  return token;
}
#endif

static const Token *fetch_token(ParseInfo *info) {
  if (info->token != NULL)
    return info->token;

  const char *start = skip_whitespaces(info->p);
  const char *p = start;
  char c = *p;
  if (isdigit(c)) {
    int base = 10;
    if (tolower(p[1]) == 'x' && isxdigit(p[2])) {
      p += 2;
      base = 16;
    }
    char *q;
    unsigned long long v = strtoull(p, &q, base);
#ifndef __NO_FLONUM
    if (*q == '.' || tolower(*q)== 'e') {
      info->p = p;
      return read_flonum(info);
    }
#endif
    Token *token = new_token(TK_FIXNUM);
    token->fixnum = v;
    info->next = q;
    return token;
#ifndef __NO_FLONUM
  } else if (c == '.' && isdigit(p[1])) {
    info->p = p;
    return read_flonum(info);
#endif
  } else if (is_label_first_chr(c)) {
    while (c = *++p, is_label_chr(c))
      ;
    Token *token = new_token(TK_LABEL);
    token->label = alloc_name(start, p, false);
    info->next = p;
    return token;
  } else if (c == '"') {
    p = skip_until_delimiter(p);
    if (p[-1] == '"') {
      const Name *label = unquote_label(start + 1, p - 1);
      if (label != NULL) {
        Token *token = new_token(TK_LABEL);
        token->label = label;
        info->next = p;
        return token;
      }
    }
  } else {
    static const char kSingleOpTable[] = "+-*/";
    static const enum TokenKind kTokenTable[] = {TK_ADD, TK_SUB, TK_MUL, TK_DIV};
    const char *q = strchr(kSingleOpTable, c);
    if (q != NULL) {
      info->next = p + 1;
      return new_token(kTokenTable[q - kSingleOpTable]);
    }
  }

  static const Token kTokUnknown = {TK_UNKNOWN};
  return &kTokUnknown;
}

static const Token *match(ParseInfo *info, enum TokenKind kind) {
  const Token *token = fetch_token(info);
  if (token->kind != kind)
    return NULL;
  info->token = NULL;
  info->p = info->next;
  return token;
}

static Expr *new_expr(enum ExprKind kind) {
  Expr *expr = malloc(sizeof(*expr));
  expr->kind = kind;
  return expr;
}

static Expr *prim(ParseInfo *info) {
  Expr *expr = NULL;
  const Token *tok;
  if ((tok = match(info, TK_LABEL)) != NULL) {
    expr = new_expr(EX_LABEL);
    expr->label = tok->label;
  } else if ((tok = match(info, TK_FIXNUM)) != NULL) {
    expr = new_expr(EX_FIXNUM);
    expr->fixnum = tok->fixnum;
#ifndef __NO_FLONUM
  } else if ((tok = match(info, TK_FLONUM)) != NULL) {
    expr = new_expr(EX_FLONUM);
    expr->flonum = tok->flonum;
#endif
  }
  return expr;
}

static Expr *unary(ParseInfo *info) {
  const Token *tok;
  if ((tok = match(info, TK_ADD)) != NULL) {
    Expr *expr = unary(info);
    if (expr == NULL)
      return NULL;
    switch (expr->kind) {
    case EX_FIXNUM:
#ifndef __NO_FLONUM
    case EX_FLONUM:
#endif
      return expr;
    default:
      {
        Expr *op = new_expr(EX_POS);
        op->unary.sub = expr;
        return op;
      }
    }
  }

  if ((tok = match(info, TK_SUB)) != NULL) {
    Expr *expr = unary(info);
    if (expr == NULL)
      return NULL;
    switch (expr->kind) {
    case EX_FIXNUM:
      expr->fixnum = -expr->fixnum;
      return expr;
#ifndef __NO_FLONUM
    case EX_FLONUM:
      expr->flonum = -expr->flonum;
      return expr;
#endif
    default:
      {
        Expr *op = new_expr(EX_NEG);
        op->unary.sub = expr;
        return op;
      }
    }
  }

  return prim(info);
}

static Expr *parse_mul(ParseInfo *info) {
  Expr *expr = unary(info);
  if (expr == NULL)
    return expr;

  const Token *tok;
  while ((tok = match(info, TK_MUL)) != NULL ||
         (tok = match(info, TK_DIV)) != NULL) {
    Expr *rhs = unary(info);
    if (rhs == NULL) {
      parse_error(info, "expression error");
      break;
    }

    Expr *lhs = expr;
    if (lhs->kind == EX_FIXNUM && rhs->kind == EX_FIXNUM) {
      switch (tok->kind) {
      case TK_MUL:  lhs->fixnum *= rhs->fixnum; break;
      case TK_DIV:  lhs->fixnum += rhs->fixnum; break;
      default:  assert(false); break;
      }
    } else {
      expr = new_expr((enum ExprKind)(tok->kind + (EX_MUL - TK_MUL)));  // Assume ExprKind is same order with TokenKind.
      expr->bop.lhs = lhs;
      expr->bop.rhs = rhs;
    }
  }
  return expr;
}

static Expr *parse_add(ParseInfo *info) {
  Expr *expr = parse_mul(info);
  if (expr == NULL)
    return expr;

  const Token *tok;
  while ((tok = match(info, TK_ADD)) != NULL ||
         (tok = match(info, TK_SUB)) != NULL) {
    Expr *rhs = parse_mul(info);
    if (rhs == NULL) {
      parse_error(info, "expression error");
      break;
    }

    Expr *lhs = expr;
    if (lhs->kind == EX_FIXNUM && rhs->kind == EX_FIXNUM) {
      switch (tok->kind) {
      case TK_ADD:  lhs->fixnum += rhs->fixnum; break;
      case TK_SUB:  lhs->fixnum += rhs->fixnum; break;
      default:  assert(false); break;
      }
    } else {
      expr = new_expr((enum ExprKind)(tok->kind + (EX_ADD - TK_ADD)));  // Assume ExprKind is same order with TokenKind.
      expr->bop.lhs = lhs;
      expr->bop.rhs = rhs;
    }
  }
  return expr;
}

static Expr *parse_expr(ParseInfo *info) {
  info->token = NULL;
  info->next = NULL;
  return parse_add(info);
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
      if (expr->kind == EX_LABEL) {
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
        expr = malloc(sizeof(*expr));
        expr->kind = EX_FIXNUM;
        expr->fixnum = 0;
      }
      return parse_indirect_register(info, expr, operand);
    }
  }

  return false;
}

static void parse_inst(ParseInfo *info, Inst *inst) {
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

int current_section = SEC_CODE;

Line *parse_line(ParseInfo *info) {
  Line *line = malloc(sizeof(*line));
  line->label = NULL;
  line->inst.op = NOOP;
  line->inst.src.type = line->inst.dst.type = NOOPERAND;
  line->dir = NODIRECTIVE;

  const char *p = skip_whitespaces(info->rawline);
  const char *q = skip_until_delimiter(p);
  const char *r = skip_whitespaces(q);
  if (*r == ':') {
    const Name *label = unquote_label(p, q);
    if (label == NULL) {
      parse_error(info, "Illegal label");
      err = true;
    } else {
      info->p = p;
      line->label = label;
      info->p = r + 1;
    }
  } else {
    if (*p == '.') {
      enum DirectiveType dir = find_directive(p + 1, q - p - 1);
      if (dir == NODIRECTIVE) {
        parse_error(info, "Unknown directive");
        return NULL;
      }
      line->dir = dir;
      info->p = r;
    } else if (*p != '\0') {
      info->p = p;
      parse_inst(info, &line->inst);
      if (*info->p != '\0' && !(*info->p == '/' && info->p[1] == '/')) {
        parse_error(info, "Syntax error");
        err = true;
      }
    }
  }
  return line;
}

static char unescape_char(ParseInfo *info) {
  const char *p = info->p;
  char c = *p++;
  switch (c) {
  case '0':  return '\0';
  case 'x':
    {
      c = 0;
      for (int i = 0; i < 2; ++i, ++p) {
        int v = xvalue(*p);
        if (v < 0)
          break;  // TODO: Error
        c = (c << 4) | v;
      }
      info->p = p - 1;
      return c;
    }
  case 'a':  return '\a';
  case 'b':  return '\b';
  case 'f':  return '\f';
  case 'n':  return '\n';
  case 'r':  return '\r';
  case 't':  return '\t';
  case 'v':  return '\v';

  default:
    parse_error(info, "Illegal escape");
    // Fallthrough
  case '\'': case '"': case '\\':
    return c;
  }
}

static size_t unescape_string(ParseInfo *info, char *dst) {
  size_t len = 0;
  for (; *info->p != '"'; ++info->p, ++len) {
    char c = *info->p;
    if (c == '\0')
      parse_error(info, "string not closed");
    if (c == '\\') {
      ++info->p;
      c = unescape_char(info);
    }
    if (dst != NULL)
      *dst++ = c;
  }
  ++info->p;
  return len;
}

void handle_directive(ParseInfo *info, enum DirectiveType dir, Vector **section_irs,
                      Table *label_table) {
  Vector *irs = section_irs[current_section];

  switch (dir) {
  case DT_ASCII:
    {
      if (*info->p != '"')
        parse_error(info, "`\"' expected");
      ++info->p;
      const char *p = info->p;
      size_t len = unescape_string(info, NULL);
      char *str = malloc(len);
      info->p = p;  // Again.
      unescape_string(info, str);

      vec_push(irs, new_ir_data(str, len));
    }
    break;

  case DT_COMM:
    {
      const Name *label = parse_label(info);
      if (label == NULL)
        parse_error(info, ".comm: label expected");
      info->p = skip_whitespaces(info->p);
      if (*info->p != ',')
        parse_error(info, ".comm: `,' expected");
      info->p = skip_whitespaces(info->p + 1);
      long count;
      if (!immediate(&info->p, &count)) {
        parse_error(info, ".comm: count expected");
        return;
      }

      long align = 0;
      if (*info->p == ',') {
        info->p = skip_whitespaces(info->p + 1);
        if (!immediate(&info->p, &align) || align < 1) {
          parse_error(info, ".comm: optional alignment expected");
          return;
        }
      }

      enum SectionType sec = SEC_BSS;
      irs = section_irs[sec];
      if (align > 1)
        vec_push(irs, new_ir_align(align));
      vec_push(irs, new_ir_label(label));
      vec_push(irs, new_ir_bss(count));

      if (!add_label_table(label_table, label, sec, true, false))
        return;
    }
    break;

  case DT_TEXT:
    current_section = SEC_CODE;
    break;

  case DT_DATA:
    current_section = SEC_DATA;
    break;

  case DT_ALIGN:
    {
      long align;
      if (!immediate(&info->p, &align))
        parse_error(info, ".align: number expected");
      vec_push(irs, new_ir_align(align));
    }
    break;
  case DT_P2ALIGN:
    {
      long align;
      if (!immediate(&info->p, &align))
        parse_error(info, ".align: number expected");
      vec_push(irs, new_ir_align(1 << align));
    }
    break;

  case DT_BYTE:
  case DT_WORD:
  case DT_LONG:
  case DT_QUAD:
    {
      Expr *expr = parse_expr(info);
      if (expr == NULL) {
        parse_error(info, "expression expected");
        break;
      }

#ifndef __NO_FLONUM
      assert(expr->kind != EX_FLONUM);
#endif
      if (expr->kind == EX_FIXNUM) {
        // TODO: Target endian.
        long value = expr->fixnum;
        int size = 1 << (dir - DT_BYTE);
        unsigned char *buf = malloc(size);
        for (int i = 0; i < size; ++i)
          buf[i] = value >> (8 * i);
        vec_push(irs, new_ir_data(buf, size));
      } else {
        vec_push(irs, new_ir_expr((enum IrKind)(IR_EXPR_BYTE + (dir - DT_BYTE)), expr));
      }
    }
    break;

#ifndef __NO_FLONUM
  case DT_FLOAT:
  case DT_DOUBLE:
    {
      Expr *expr = parse_expr(info);
      if (expr == NULL) {
        parse_error(info, "expression expected");
        break;
      }

      double value;
      switch (expr->kind) {
      case EX_FIXNUM:  value = expr->fixnum; break;
      case EX_FLONUM:  value = expr->flonum; break;
      default:
        assert(false);
        value = -1;
        break;
      }
      int size = dir == DT_FLOAT ? sizeof(float) : sizeof(double);
      unsigned char *buf = malloc(size);
      if (dir == DT_FLOAT) {
        float fval = value;
        memcpy(buf, (void*)&fval, sizeof(fval));  // TODO: Endian
      } else {
        memcpy(buf, (void*)&value, sizeof(value));  // TODO: Endian
      }
      vec_push(irs, new_ir_data(buf, size));
    }
    break;
#endif

  case DT_GLOBL:
  case DT_LOCAL:
    {
      const Name *label = parse_label(info);
      if (label == NULL) {
        char buf[32];
        snprintf(buf, sizeof(buf), "%s: label expected", dir == DT_GLOBL ? ".globl" : ".local");
        parse_error(info, buf);
        return;
      }

      if (!add_label_table(label_table, label, current_section, false, dir == DT_GLOBL))
        err = true;
    }
    break;

  case DT_SECTION:
    {
      const Name *name = parse_section_name(info);
      if (name == NULL) {
        parse_error(info, ".section: section name expected");
        return;
      }
      if (equal_name(name, alloc_name(".rodata", NULL, false))) {
        current_section = SEC_RODATA;
      } else {
        parse_error(info, "Unknown section name");
        return;
      }
    }
    break;

  case DT_EXTERN:
    break;

  default:
    parse_error(info, "Unhandled directive");
    break;
  }
}
