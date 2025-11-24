#include "../config.h"
#include "parse_asm.h"

#include <assert.h>
#include <ctype.h>
#include <stdint.h>
#include <stdlib.h>  // strtoul
#include <string.h>
#include <strings.h>

#if __STDC_NO_VLA__
#include <alloca.h>
#endif

#include "ir_asm.h"
#include "table.h"
#include "util.h"

// #include "inst.h"  // To enable code interpolation on a text editor.
#if XCC_TARGET_ARCH == XCC_ARCH_X64
#include "./arch/x64/inst.h"
#elif XCC_TARGET_ARCH == XCC_ARCH_AARCH64
#include "./arch/aarch64/inst.h"
#elif XCC_TARGET_ARCH == XCC_ARCH_RISCV64
#include "./arch/riscv64/inst.h"
#else
#error "Unsupported architecture"
#endif

#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
const char kSegText[] = "__TEXT";
const char kSecText[] = "__text";
const char kSegRodata[] = "__DATA";
const char kSecRodata[] = "__const";
const char kSegData[] = "__DATA";
const char kSecData[] = "__data";
const char kSegBss[] = "__DATA";
const char kSecBss[] = "__bss";
#else
const char kSecText[] = ".text";
const char kSecRodata[] = ".rodata";
const char kSecData[] = ".data";
const char kSecBss[] = ".bss";
#endif

#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
#define WEAK  "weak_definition"
#else
#define WEAK  "weak"
#endif

static const char *kDirectiveTable[] = {
  "ascii",
  "string",
  "section",
  "text",
  "data",
  "bss",
  "align",
  "p2align",
  "type",
  "byte",
  "short",
  "long",
  "quad",
  "comm",
  "zero",
  "globl",
  "local",
  WEAK,
  "extern",
#ifndef __NO_FLONUM
  "float",
  "double",
#endif
#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
  "subsections_via_symbols",
#endif
};

SectionInfo *get_section_info(ParseInfo *info, const char *name, const char *segname, int flag) {
  const Name *key = alloc_name(name, NULL, true);
  SectionInfo *section;
  if (!table_try_get(info->section_infos, key, (void**)&section)) {
    section = calloc_or_die(sizeof(*section));
    section->name = key;
    section->segname = segname;
    section->irs = new_vector();
    section->flag = flag;
    section->index = 0;
    section->start_address = 0;
    section->ds = NULL;
    section->align = 1;
    section->bss_size = 0;
    section->rela_count = 0;
    section->rela_buf = NULL;
    if (!(flag & SF_BSS)) {
      DataStorage *ds = calloc_or_die(sizeof(*ds));
      data_init(ds);
      section->ds = ds;
    }
    table_put(info->section_infos, key, section);
  }
  return section;
}

SectionInfo *set_current_section(ParseInfo *info, const char *name, const char *segname, int flag) {
  SectionInfo *section = get_section_info(info, name, segname, flag);
  info->current_section = section;
  return section;
}

static LabelInfo *new_label(SectionInfo *section) {
  LabelInfo *info = calloc_or_die(sizeof(*info));
  info->section = section;
  info->flag = 0;
  info->address = 0;
  info->kind = LK_NONE;
  info->size = 0;
  info->align = 0;
  return info;
}

LabelInfo *add_label_table(Table *label_table, const Name *label, SectionInfo *section, bool define,
                           bool global) {
  LabelInfo *info = table_get(label_table, label);
  if (info != NULL) {
    if (define) {
      if ((info->flag & LF_DEFINED) != 0) {
        fprintf(stderr, "`%.*s' already defined\n", NAMES(label));
        return NULL;
      }
      info->address = 1;
      info->section = section;
    }
  } else {
    info = new_label(section);
    table_put(label_table, label, info);
  }
  if (define)
    info->flag |= LF_DEFINED;
  if (global)
    info->flag |= LF_GLOBAL;
  return info;
}

bool parse_error(ParseInfo *info, const char *message) {
  fprintf(stderr, "%s(%d): %s\n", info->filename, info->lineno, message);
  fprintf(stderr, "%s\n", info->rawline);
  ++info->error_count;
  return false;
}

static enum DirectiveType find_directive(const char *p, size_t n) {
  const char **table = kDirectiveTable;
  for (size_t i = 0; i < ARRAY_SIZE(kDirectiveTable); ++i) {
    const char *name = table[i];
    if (strncasecmp(p, name, n) == 0 && name[n] == '\0') {
      return i + 1;
    }
  }
  return NODIRECTIVE;
}

bool immediate(const char **pp, int64_t *value) {
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

bool is_label_first_chr(char c) {
  return isalpha(c) || c == '_' || c == '.';
}

bool is_label_chr(char c) {
  return is_label_first_chr(c) || isdigit(c);
}

static const char *get_label_end(ParseInfo *info) {
  const char *start = info->p;
  const char *p = start;
  if (*p == '"') {
    ++p;
    for (;;) {
      char c = *p;
      if (c == '\0') {
        parse_error(info, "string not closed");
        break;
      }

      ++p;
      if (c == '"')
        break;
      if (c == '\\')
        ++p;
    }
    start += 2;
  } else {
    const unsigned char *q = (const unsigned char*)p;
    int ucc = 0;
    for (;;) {
      int uc = *++q;
      if (ucc > 0) {
        if (!isutf8follow(uc)) {
          parse_error(info, "illegal byte sequence");
          return NULL;
        }
        --ucc;
        continue;
      }
      if ((ucc = isutf8first(uc) - 1) > 0)
        continue;
      if (!is_label_chr(uc))
        break;
    }
    p = (const char*)q;
  }
  if (p <= start)
    parse_error(info, "empty label");
  return p;
}

const Name *unquote_label(const char *p, const char *q) {
  if (*p != '"')
    return alloc_name(p, q, false);
  if (q[-1] != '"' || q == p + 2)
    return NULL;
  // TODO: Unescape
  return alloc_name(p + 1, q - 1, false);
}

static const Name *parse_label(ParseInfo *info) {
  const char *start = info->p;
  const char *p = get_label_end(info);
  if (p == start)
    return NULL;

  info->p = p;
  return unquote_label(start, p);
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

enum TokenKind {
  TK_EOF,
  TK_UNKNOWN,
  TK_LABEL,
  TK_FIXNUM,
  TK_ADD,
  TK_SUB,
  TK_MUL,
  TK_DIV,
  TK_FLONUM,
};

typedef struct Token {
  enum TokenKind kind;
  union {
    struct {
      const Name *name;
    } label;
    int64_t fixnum;
#ifndef __NO_FLONUM
    Flonum flonum;
#endif
  };
} Token;

static Token *new_token(enum TokenKind kind) {
  Token *token = calloc_or_die(sizeof(*token));
  token->kind = kind;
  return token;
}

#ifndef __NO_FLONUM
static Token *read_flonum(ParseInfo *info, int base) {
  const char *start = info->p;
  char *next;
#ifdef __XCC
  // long double in XCC is same as double, and if the target platform uses
  // system library, it makes discrepancy.
  Flonum val = strtod(start, &next);
#else
  Flonum val = strtold(start, &next);
#endif
  Token *tok = new_token(TK_FLONUM);
  tok->flonum = val;
  info->p = next;

  if (base == 16) {
    // Check exponent part exists.
    const char *q;
    for (q = start; q < next; ++q) {
      if (tolower(*q) == 'p')
        break;
    }
    if (q >= next) {
      parse_error(info, "hex float literal must have exponent part");
    }
  }

  return tok;
}
#endif

static const Token *fetch_token(ParseInfo *info) {
  static const Token kTokEOF = {.kind = TK_EOF};
  const char *start = skip_whitespaces(info->p);
  const char *p = start;
  char c = *p;
  switch (c) {
  case '\0':
    return &kTokEOF;
  case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
    {
      int base = 10;
      if (c == '0' && tolower(p[1]) == 'x') {
#ifndef __NO_FLONUM
        if (c == '.')  // Hex float literal.
          return read_flonum(info, 16);
#endif
        if (isxdigit(p[2])) {
          p += 2;
          base = 16;
        }
      }
      char *q;
      unsigned long long v = strtoull(p, &q, base);
#ifndef __NO_FLONUM
      if (*q == '.' || tolower(*q) == 'e') {
        info->p = p;
        return read_flonum(info, 10);
      }
#endif
      Token *token = new_token(TK_FIXNUM);
      token->fixnum = v;
      info->p = q;
      return token;
    }
  case '"':
    {
      info->p = p;
      const Name *label = parse_label(info);
      if (label != NULL) {
        Token *token = new_token(TK_LABEL);
        token->label.name = label;
        return token;
      }
    }
    break;
  case '+': case '-': case '*': case '/':
    {
      enum TokenKind kind;
      switch (c) {
      default: assert(false); // To suppress warning.
      case '+':  kind = TK_ADD; break;
      case '-':  kind = TK_SUB; break;
      case '*':  kind = TK_MUL; break;
      case '/':  kind = TK_DIV; break;
      }
      info->p = p + 1;
      return new_token(kind);
    }
#ifndef __NO_FLONUM
  case '.':
    if (isdigit(p[1])) {
      info->p = p;
      return read_flonum(info, 10);
    }
    break;
#endif
  default: break;
  }

  if (is_label_first_chr(c)) {
    const char *label = p;
    while (c = *++p, is_label_chr(c))
      ;
    Token *token = new_token(TK_LABEL);
    token->label.name = alloc_name(label, p, false);
    info->p = p;
    return token;
  }

  static const Token kTokUnknown = {.kind = TK_UNKNOWN};
  return &kTokUnknown;
}

static const Token *match(ParseInfo *info, enum TokenKind kind) {
  const Token *token = info->prefetched;
  if (token == NULL)
    token = fetch_token(info);

  if (token->kind != kind) {
    info->prefetched = token;
    return NULL;
  }
  info->prefetched = NULL;
  return token;
}

Expr *new_expr(enum ExprKind kind) {
  Expr *expr = calloc_or_die(sizeof(*expr));
  expr->kind = kind;
  return expr;
}

static Expr *prim(ParseInfo *info) {
  Expr *expr = NULL;
  const Token *tok;
  if ((tok = match(info, TK_LABEL)) != NULL) {
    expr = new_expr(EX_LABEL);
    expr->label.name = tok->label.name;
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
    case EX_FLONUM:
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
      // Assume ExprKind is same order with TokenKind.
      expr = new_expr((enum ExprKind)(tok->kind + (EX_ADD - TK_ADD)));
      expr->bop.lhs = lhs;
      expr->bop.rhs = rhs;
    }
  }
  return expr;
}

Expr *parse_expr(ParseInfo *info) {
  info->prefetched = NULL;
  return parse_add(info);
}

#define R_NOOP  0

#if XCC_TARGET_ARCH == XCC_ARCH_RISCV64
static const Name *alloc_dummy_label(void) {
  // TODO: Ensure label is unique.
  static int label_no;
  ++label_no;
  char buf[2 + sizeof(int) * 3 + 1];
  snprintf(buf, sizeof(buf), "._%d", label_no);
  return alloc_name(buf, NULL, true);
}
#endif

static /*enum RawOpcode*/int find_raw_opcode(ParseInfo *info) {
  const char *p = info->p;
  const char *start = p;

  while (isalnum(*p) || *p == '.')
    ++p;
  if (*p == '\0' || isspace(*p)) {
    size_t n = p - start;
    for (int i = 0; ; ++i) {
      const char *name = kRawOpTable[i];
      if (name == NULL)
        break;
      size_t len = strlen(name);
      if (n == len && strncasecmp(start, name, n) == 0) {
        info->p = skip_whitespaces(p);
        return i + 1;
      }
    }
  }
  return R_NOOP;
}

static bool parse_inst(ParseInfo *info, Line *line) {
  Inst inst;
  inst.op = NOOP;
  for (int i = 0; i < (int)ARRAY_SIZE(inst.opr); ++i)
    inst.opr[i].type = NOOPERAND;

  /*enum RawOpcode*/int op = find_raw_opcode(info);
  if (op != R_NOOP) {
    const ParseInstTable *pt = &kParseInstTable[op];
    int n = pt->count;
#if __STDC_NO_VLA__
    const ParseOpArray **candidates = alloca(n * sizeof(*candidates));
    assert(candidates != NULL);
#else
    const ParseOpArray *candidates[n];
#endif
    memcpy(candidates, pt->array, n * sizeof(*candidates));
    for (int i = 0; i < (int)ARRAY_SIZE(inst.opr); ++i) {
      unsigned int opr_flags = 0;
      for (int j = 0; j < n; ++j)
        opr_flags |= candidates[j]->opr_flags[i];
      if (opr_flags == 0)
        break;

      if (i > 0) {
        if (*info->p != ',') {
          if (candidates[0]->opr_flags[i] == 0)
            break;
          parse_error(info, "comma expected");
          return false;  // Error
        }
        info->p = skip_whitespaces(info->p + 1);
      }

      Operand *opr = &inst.opr[i];
      const char *before = info->p;
      unsigned int result = parse_operand(info, opr_flags, opr);
      if (result == 0) {
        parse_error(info, "illegal operand");
        info->p = before;
        return false;  // Error
      }

      for (int j = 0; j < n; ++j) {
        if ((candidates[j]->opr_flags[i] & result) == 0) {
          memmove(&candidates[j], &candidates[j + 1], (n - j - 1) * sizeof(*candidates));
          --n;
          --j;
        }
      }

      info->p = skip_whitespaces(info->p);
    }

    if (n > 0) {
      inst.op = candidates[0]->op;

#if XCC_TARGET_ARCH == XCC_ARCH_RISCV64
      // Tweak for instruction.
      switch (inst.op) {
      case LA:
        // Store corresponding label to opr3.
        if (line->label == NULL) {
          // Generate unique label.
          const Name *label = alloc_dummy_label();
          line->label = label;
        }
        if (inst.opr[2].type == NOOPERAND) {
          Expr *expr = new_expr(EX_LABEL);
          expr->label.name = line->label;

          Operand *opr = &inst.opr[2];
          opr->type = DIRECT;
          opr->direct.expr = expr;
        }
        break;
      default: break;
      }
#endif
    }
  }

  if (inst.op != NOOP) {
    Inst *cloned = calloc_or_die(sizeof(inst));
    *cloned = inst;
    line->inst = cloned;
  }
  return true;
}

void parse_set_p(ParseInfo *info, const char *p) {
  info->p = p;
  info->prefetched = NULL;
}

static char unescape_char(ParseInfo *info) {
  const char *p = info->p;
  char c = *p++;
  switch (c) {
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    {
      // Octal, but accepts '8' and '9'.
      int x = 0;
      --p;
      for (int i = 0; i < 3; ++i, ++p) {  // "The numeric code is 3 octal digits."
        char cc = *p;
        if (!isdigit(cc))
          break;
        x = (x << 3) | (cc - '0');
      }
      info->p = p - 1;
      return x;
    }
  case 'x':
    {
      int x = 0;
      for (;; ++p) {  // "All trailing hex digits are combined."
        int v = xvalue(*p);
        if (v < 0)
          break;  // TODO: Error
        x = (x << 4) | v;
      }
      info->p = p - 1;
      return x;
    }
  case 'a':  return '\a';
  case 'b':  return '\b';
  case 'f':  return '\f';
  case 'n':  return '\n';
  case 'r':  return '\r';
  case 't':  return '\t';
  case 'v':  return '\v';

  default:
    parse_error(info, "illegal escape");
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

#if XCC_TARGET_PLATFORM != XCC_PLATFORM_APPLE
static char *parse_string(ParseInfo *info) {
  const char *start = skip_whitespaces(info->p);
  if (*start != '"')
    return NULL;

  info->p = start;
  const char *p = get_label_end(info);
  if (p == start)
    return NULL;

  info->p = p;

  const char *s = start + 1, *e = p - 1;
  return strndup(s, e - s);
}

static uint32_t parse_section_flag(ParseInfo *info) {
  uint32_t flag = 0;
  char *flag_str = parse_string(info);
  if (flag_str == NULL) {
    parse_error(info, ".section: flag string expected");
  } else {
    for (char *p = flag_str; *p != '\0'; ++p) {
      switch (*p) {
      case 'a':  /*ignore*/ break;
      case 'w':  flag |= SF_WRITABLE; break;
      case 'x':  flag |= SF_EXECUTABLE; break;
      default:
        parse_error(info, ".section: illegal flag character");
        break;
      }
    }
  }
  return flag;
}
#endif

static bool dir_string(ParseInfo *info, enum DirectiveType dir) {
  if (*info->p != '"')
    return parse_error(info, "`\"' expected");
  ++info->p;
  const char *p = info->p;
  size_t len = unescape_string(info, NULL);
  if (dir == DT_STRING)
    ++len;
  char *str = calloc_or_die(len);
  info->p = p;  // Again.
  unescape_string(info, str);

  SectionInfo *section = info->current_section;
  Vector *irs = section->irs;
  vec_push(irs, new_ir_data(str, len));
  return true;
}

static bool dir_comm(ParseInfo *info, enum DirectiveType dir) {
  UNUSED(dir);
  const Name *name = parse_label(info);
  if (name == NULL)
    return parse_error(info, ".comm: label expected");
  info->p = skip_whitespaces(info->p);
  if (*info->p != ',')
    return parse_error(info, ".comm: `,' expected");
  info->p = skip_whitespaces(info->p + 1);
  int64_t size;
  if (!immediate(&info->p, &size) || size <= 0)
    return parse_error(info, ".comm: size expected");

  int64_t align = 0;
  if (*info->p == ',') {
    info->p = skip_whitespaces(info->p + 1);
    if (!immediate(&info->p, &align) ||
#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
        align < 0
#else
        align < 1
#endif
    ) {
      return parse_error(info, ".comm: optional alignment expected");
    }
#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
    // p2align on macOS.
    align = 1 << align;
#endif
  }

  SectionInfo *section = get_section_info(info, kSecBss, kSegBss, SF_BSS | SF_WRITABLE);
  Vector *irs = section->irs;
  if (align > 1)
    vec_push(irs, new_ir_align(align));
  vec_push(irs, new_ir_label(name));
  vec_push(irs, new_ir_bss(size));

  LabelInfo *label = add_label_table(info->label_table, name, section, true, false);
  if (label == NULL)
    return false;
  label->size = size;
  label->align = align;
  label->flag |= LF_COMM;
  return true;
}

static bool dir_zero(ParseInfo *info, enum DirectiveType dir) {
  UNUSED(dir);
  int64_t num;
  if (!immediate(&info->p, &num))
    return parse_error(info, ".zero: number expected");

  SectionInfo *section = info->current_section;
  Vector *irs = section->irs;
  vec_push(irs, new_ir_zero(num));
  return true;
}

static bool dir_text(ParseInfo *info, enum DirectiveType dir) {
  UNUSED(dir);
  set_current_section(info, kSecText, kSegText, SF_EXECUTABLE);
  return true;
}

static bool dir_data(ParseInfo *info, enum DirectiveType dir) {
  UNUSED(dir);
  set_current_section(info, kSecData, kSegData, SF_WRITABLE);
  return true;
}

static bool dir_bss(ParseInfo *info, enum DirectiveType dir) {
  UNUSED(dir);
  set_current_section(info, kSecBss, kSegBss, SF_BSS | SF_WRITABLE);
  return true;
}

static bool dir_align(ParseInfo *info, enum DirectiveType dir) {
  int64_t align;
  if (!immediate(&info->p, &align))
    return parse_error(info, ".align: number expected");
  if (dir == DT_P2ALIGN)
    align = 1 << align;

  SectionInfo *section = info->current_section;
  Vector *irs = section->irs;
  vec_push(irs, new_ir_align(align));
  return true;
}

static bool dir_type(ParseInfo *info, enum DirectiveType dir) {
  UNUSED(dir);
  const Name *name = parse_label(info);
  if (name == NULL)
    return parse_error(info, ".type: label expected");
  if (*info->p != ',')
    return parse_error(info, ".type: `,' expected");
  info->p = skip_whitespaces(info->p + 1);
  enum LabelKind kind = LK_NONE;
  if (strcmp(info->p, "@function") == 0) {
    kind = LK_FUNC;
    info->p += 9;
  } else if (strcmp(info->p, "@object") == 0) {
    kind = LK_OBJECT;
    info->p += 7;
  } else {
    return parse_error(info, "illegal .type");
  }

  SectionInfo *section = info->current_section;
  LabelInfo *label = add_label_table(info->label_table, name, section, false, false);
  if (label != NULL) {
    label->kind = kind;
  }
  return true;
}

static bool dir_bytes(ParseInfo *info, enum DirectiveType dir) {
  Expr *expr = parse_expr(info);
  if (expr == NULL)
    return parse_error(info, "expression expected");

  SectionInfo *section = info->current_section;
  Vector *irs = section->irs;

  assert(expr->kind != EX_FLONUM);
  if (expr->kind == EX_FIXNUM) {
    // TODO: Target endian.
    long value = expr->fixnum;
    int size = 1 << (dir - DT_BYTE);
    unsigned char *buf = malloc_or_die(size);
    for (int i = 0; i < size; ++i)
      buf[i] = value >> (8 * i);
    vec_push(irs, new_ir_data(buf, size));
  } else {
    vec_push(irs, new_ir_expr((enum IrKind)(IR_EXPR_BYTE + (dir - DT_BYTE)), expr));
  }
  return true;
}

#ifndef __NO_FLONUM
static bool dir_float(ParseInfo *info, enum DirectiveType dir) {
  Expr *expr = parse_expr(info);
  if (expr == NULL)
    return parse_error(info, "expression expected");

  Flonum value;
  switch (expr->kind) {
  case EX_FIXNUM:  value = expr->fixnum; break;
  case EX_FLONUM:  value = expr->flonum; break;
  default:
    assert(false);
    value = -1;
    break;
  }
  int size;
  switch (dir) {
  default: assert(false); // Fallthrough
  case DT_DOUBLE:  size = sizeof(double); break;
  case DT_FLOAT:   size = sizeof(float); break;
  }
  unsigned char *buf = malloc_or_die(size);
  if (dir == DT_FLOAT) {
    float fval = value;
    memcpy(buf, (void*)&fval, sizeof(fval));  // TODO: Endian
  } else {
    double dval = value;
    memcpy(buf, (void*)&dval, sizeof(dval));  // TODO: Endian
  }

  SectionInfo *section = info->current_section;
  Vector *irs = section->irs;
  vec_push(irs, new_ir_data(buf, size));
  return true;
}
#endif

static bool dir_label_attrib(ParseInfo *info, enum DirectiveType dir) {
  const Name *name = parse_label(info);
  if (name == NULL) {
    char buf[32];
    snprintf(buf, sizeof(buf), "%s: label expected", dir == DT_GLOBL ? ".globl" : ".local");
    return parse_error(info, buf);
  }

  SectionInfo *section = info->current_section;
  LabelInfo *label = add_label_table(info->label_table, name, section, false, dir == DT_GLOBL);
  if (label == NULL) {
    ++info->error_count;
  } else {
    if (dir == DT_WEAK)
      label->flag |= LF_WEAK;
  }
  return true;
}

static bool dir_section(ParseInfo *info, enum DirectiveType dir) {
  UNUSED(dir);
  const Name *name = parse_section_name(info);
  if (name == NULL)
    return parse_error(info, ".section: section name expected");
#if XCC_TARGET_PLATFORM != XCC_PLATFORM_APPLE
  int flag = 0;
  const char *p = skip_whitespaces(info->p);
  if (*p == ',') {
    info->p = p + 1;
    flag = parse_section_flag(info);
  }

  char *sectname = strndup(name->chars, name->bytes);
  set_current_section(info, sectname, kSegRodata, flag);
#else
  const char *p = skip_whitespaces(info->p);
  if (*p != ',')
    return parse_error(info, "`,' expected");
  info->p = skip_whitespaces(p + 1);
  const Name *name2 = parse_section_name(info);
  if (name2 == NULL)
    return parse_error(info, ".section: section name expected");

  int flag = 0;
  p = skip_whitespaces(info->p);
  if (*p == ',') {
    info->p = p + 1;
    const Name *modname = parse_section_name(info);
    if (modname != NULL) {
      if (equal_name(modname, alloc_name("mod_init_funcs", NULL, false))) {
        flag |= SF_INIT_FUNCS;
      } else if (equal_name(modname, alloc_name("cstring_literals", NULL, false))) {
        flag |= SF_CSTRLITERALS;
      }
    }
    if (flag == 0)
      return parse_error(info, ".section: section name expected");
  }

  char *segname = strndup(name->chars, name->bytes);
  char *sectname = strndup(name2->chars, name2->bytes);
  set_current_section(info, sectname, segname, flag);
#endif
  return true;
}

#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
static bool dir_subsections_via_symbols(ParseInfo *info, enum DirectiveType dir) {
  // TODO: Handle this flag.
  UNUSED(info);
  UNUSED(dir);
  return true;
}
#endif

static inline bool handle_directive(ParseInfo *info, enum DirectiveType dir) {
  typedef bool (*DirectiveFunc)(ParseInfo *info, enum DirectiveType dir);

  static const DirectiveFunc kDirectiveFuncTable[] = {
    [NODIRECTIVE] = NULL,
    [DT_ASCII] = dir_string, [DT_STRING] = dir_string,
    [DT_SECTION] = dir_section,
    [DT_TEXT] = dir_text,
    [DT_DATA] = dir_data,
    [DT_BSS] = dir_bss,
    [DT_ALIGN] = dir_align, [DT_P2ALIGN] = dir_align,
    [DT_TYPE] = dir_type,
    [DT_BYTE] = dir_bytes, [DT_SHORT] = dir_bytes, [DT_LONG] = dir_bytes, [DT_QUAD] = dir_bytes,
    [DT_COMM] = dir_comm,
    [DT_ZERO] = dir_zero,
    [DT_GLOBL] = dir_label_attrib, [DT_LOCAL] = dir_label_attrib, [DT_WEAK] = dir_label_attrib,
    [DT_EXTERN] = NULL,
  #ifndef __NO_FLONUM
    [DT_FLOAT] = dir_float, [DT_DOUBLE] = dir_float,
  #endif
  #if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
    [DT_SUBSECTIONS_VIA_SYMBOLS] = dir_subsections_via_symbols,
  #endif
  };

  DirectiveFunc func = kDirectiveFuncTable[dir];
  if (func == NULL)
    return true;
  return (*func)(info, dir);
}

bool parse_line(Line *line, ParseInfo *info) {
  memset(line, 0, sizeof(*line));
  line->label = NULL;
  line->inst = NULL;
  line->dir = NODIRECTIVE;

  const char *p = skip_whitespaces(info->p);
  info->p = p;
  const char *q = get_label_end(info);
  const char *r = skip_whitespaces(q);
  if (*r == ':') {
    const Name *label = unquote_label(p, q);
    if (label == NULL)
      return parse_error(info, "illegal label");
    line->label = label;
    info->p = p = skip_whitespaces(r + 1);
  } else if (*p == '.') {
    enum DirectiveType dir = find_directive(p + 1, q - p - 1);
    if (dir == NODIRECTIVE) {
      parse_error(info, "unknown directive");
      return false;
    }
    line->dir = dir;
    info->p = r;
    return handle_directive(info, dir);
  }

  if (*p != '\0') {
    info->p = p;
    if (!parse_inst(info, line))
      return false;
  }
  return true;
}

Value calc_expr(Table *label_table, const Expr *expr) {
  assert(expr != NULL);
  switch (expr->kind) {
  case EX_LABEL:
    return (Value){.label = expr->label.name, .offset = 0};
  case EX_FIXNUM:
    return (Value){.label = NULL, .offset = expr->fixnum};
  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
  case EX_DIV:
    {
      Value lhs = calc_expr(label_table, expr->bop.lhs);
      Value rhs = calc_expr(label_table, expr->bop.rhs);
      if (rhs.label != NULL) {
        if (expr->kind == EX_SUB && lhs.label != NULL) {
          LabelInfo *llabel, *rlabel;
          if (table_try_get(label_table, lhs.label, (void**)&llabel) &&
              table_try_get(label_table, rhs.label, (void**)&rlabel)) {
            return (Value){.label = NULL, .offset = llabel->address - rlabel->address};
          } else {
            error("Unresolved");
          }
        }
        if (expr->kind != EX_ADD || lhs.label != NULL) {
          error("Illegal expression");
        }
        // offset + label
        return (Value){.label = rhs.label, .offset = lhs.offset + rhs.offset};
      }
      if (lhs.label != NULL) {
        if (expr->kind != EX_ADD) {
          error("Illegal expression");
        }
        // label + offset
        return (Value){.label = lhs.label, .offset = lhs.offset + rhs.offset};
      }

      assert(lhs.label == NULL && rhs.label == NULL);
      switch (expr->kind) {
      case EX_ADD:  lhs.offset += rhs.offset; break;
      case EX_SUB:  lhs.offset -= rhs.offset; break;
      case EX_MUL:  lhs.offset *= rhs.offset; break;
      case EX_DIV:  lhs.offset /= rhs.offset; break;
      default: assert(false); break;
      }
      return lhs;
    }

  case EX_POS:
  case EX_NEG:
    {
      Value value = calc_expr(label_table, expr->unary.sub);
      if (value.label != NULL) {
        error("Illegal expression");
      }
      if (expr->kind == EX_NEG)
        value.offset = -value.offset;
      return value;
    }

  default: assert(false); break;
  }
  return (Value){.label = NULL, .offset = 0};
}
